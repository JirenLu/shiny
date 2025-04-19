library(shiny)
library(shinyWidgets)
library(leaflet)
library(lubridate)
library(reticulate)
library(readr)
library(dplyr)
library(hms)

# 设置 Python 环境
use_virtualenv("r-reticulate", required = TRUE)

# 导入 Python 模块
joblib <- import("joblib")
pd <- import("pandas")

# 加载 Python 天气模块
source_python("weather_fetch.py")

# 加载机场信息
airport_info <- read_csv("data/airports_info_.csv")

# 查找经纬度函数
get_coords <- function(iata_code) {
  row <- airport_info[airport_info$Airport == toupper(iata_code), ]
  if (nrow(row) == 0) return(c(NA, NA))
  return(c(row$Latitude, row$Longitude))
}

# 加载模型（带天气）
models <- list(
  cancel_weather = joblib$load("model/pipe_cancel_weather.pkl")
)




# ==================== UI ====================
ui <- fluidPage(
  tags$div(style = "display: none;", textOutput("page")),
  tags$head(
    tags$style(HTML("body {
        background-image: url('background3.jpg');
        background-size: cover;
        background-attachment: fixed;
        background-position: center center;
        background-repeat: no-repeat;
        color: white;
      }
      .box-style {
        background-color: rgba(0, 0, 0, 0.4);
        backdrop-filter: blur(12px);
        padding: 25px;
        border-radius: 20px;
        width: 500px;
        color: white;
        box-shadow: 0 8px 30px rgba(0, 0, 0, 0.3);
      }"))
  ),
  conditionalPanel(
    condition = "output.page == 'home'",
    absolutePanel(top = 20, left = 20, fixed = TRUE, draggable = FALSE,
                  actionButton("suggest_btn",
                               label = tagList(
                                 tags$img(src = "smallplane.png", style = "width: 60px; height: 60px; margin-right: 3px;"),
                                 "Travel Advice"
                               ),
                               style = "background-color: transparent; border: none; color: white; font-weight: bold;"
                  )
    ),
    div(
      style = "height: 100vh; display: flex; flex-direction: column; justify-content: center; align-items: center;",
      div(style = "text-align: center; margin-bottom: 40px;",
          h1("SkyCast", style = "font-size: 70px; font-weight: bold; color: white; text-shadow: 2px 2px 6px rgba(0,0,0,0.6);"),
          h4("Predict your flight. Manage your life.", style = "font-size: 24px; font-weight: normal; color: white; text-shadow: 1px 1px 4px rgba(0,0,0,0.5);")
      ),
      div(class = "box-style",
          fluidRow(
            column(6, textInput("origin", "Departure Airport*", placeholder = "JFK")),
            column(6, textInput("dest", "Destination Airport*", placeholder = "LAX"))
          ),
          fluidRow(
            column(6, dateInput("flight_date", "Flight Date*", value = Sys.Date())),
            column(6, textInput("carrier", "Carrier*", placeholder = "AA"))
          ),
          fluidRow(
            column(6, textInput("dep_time", "Departure Time*", placeholder = "08:00")),
            column(6, textInput("arr_time", "Arrival Time*", placeholder = "11:30"))
          ),
          div(style = "text-align: center; margin-top: 20px;",
              actionButton("predict_btn", "\ud83d\udd0d Predict", class = "btn btn-danger btn-lg")
          )
      ),
      tags$div(
        style = "position: fixed; bottom: 10px; width: 100%; text-align: center; font-size: 14px; color: #eee;",
        HTML("Contributors: Yifan Chen, Jiapeng Wang, Zhixing Liu<br/>If you have any questions, contact: <a href='mailto:ychen2533@wisc.edu' style='color: #ccc;'>ychen2533@wisc.edu</a><br/>© 2025 All rights reserved.")
      )
    )
  ),
  conditionalPanel(
    condition = "output.page == 'result'",
    div(class = "box-style", style = "margin: 60px auto; max-width: 600px;",
        h2("预测结果", style = "color: white; text-align: center;"),
        verbatimTextOutput("prediction_output"),
        actionButton("back_btn", "返回主页", class = "btn btn-secondary", style = "margin-top: 20px; display: block; margin-left: auto; margin-right: auto;")
    )
  ),
  conditionalPanel(
    condition = "output.page == 'advice'",
    div(class = "box-style", style = "margin: 100px auto; max-width: 600px;",
        h2("\u2708\ufe0f 出行建议", style = "font-weight: bold; color: white; text-align: center; margin-bottom: 20px;"),
        
        div(style = "text-align: center; margin-top: 30px;",
            actionButton("back_btn", "Return", class = "btn btn-secondary")
        )
    )
  )
)

# ==================== Server ====================
server <- function(input, output, session) {
  current_page <- reactiveVal("home")
  output$page <- reactive(current_page())
  outputOptions(output, "page", suspendWhenHidden = FALSE)
  
  observeEvent(input$back_btn, { current_page("home") })
  observeEvent(input$suggest_btn, { current_page("advice") })
  
  observeEvent(input$predict_btn, {
    tryCatch({
      req(input$origin, input$dest, input$flight_date, input$carrier, input$dep_time, input$arr_time)
      
      coords_o <- get_coords(input$origin)
      coords_d <- get_coords(input$dest)
      
      if (any(is.na(c(coords_o, coords_d)))) {
        showModal(modalDialog(title = "错误", "无法识别输入的机场代码，请检查 IATA 码是否正确。", easyClose = TRUE))
        return()
      }
      
      weather_info <- get_weather_features_for_user_input(
        coords_o[1], coords_o[2],
        coords_d[1], coords_d[2],
        format(input$flight_date, "%Y-%m-%d")
      )
      
      if (is.null(weather_info)) {
        showModal(modalDialog(title = "天气信息获取失败", "无法获取指定日期的天气数据。", easyClose = TRUE))
        return()
      }
      
      dep_time_min <- as.numeric(hms::as_hms(paste0(input$dep_time, ":00"))) / 60
      arr_time_min <- as.numeric(hms::as_hms(paste0(input$arr_time, ":00"))) / 60
      sch_duration <- arr_time_min - dep_time_min
      if (sch_duration < 0) sch_duration <- sch_duration + 1440
      
      dep_hour <- hour(hms::as_hms(paste0(input$dep_time, ":00")))
      arr_hour <- hour(hms::as_hms(paste0(input$arr_time, ":00")))
      
      input_df <- tibble::tibble(
        WEEK = isoweek(input$flight_date),
        MKT_AIRLINE = toupper(input$carrier),
        ORIGIN_IATA = toupper(input$origin),
        DEST_IATA = toupper(input$dest),
        SCH_DEP_TIME = dep_time_min,
        SCH_ARR_TIME = arr_time_min,
        SCH_DURATION = sch_duration,
        DISTANCE = 0,
        ORIGIN_TYPE = "",
        ORIGIN_ELEV = 0,
        DEST_TYPE = "",
        DEST_ELEV = 0,
        IS_WEEKEND = wday(input$flight_date) %in% c(1, 7),
        IS_HOLIDAY = FALSE,
        DEP_HOUR = dep_hour,
        ARR_HOUR = arr_hour
      ) %>% bind_cols(as_tibble(py_to_r(weather_info)))
      
      df_py <- pd$DataFrame(r_to_py(input_df))
      pred <- models$cancel_weather$predict(df_py)
      
      output$prediction_output <- renderPrint({
        cat("✈️ 预测结果：\n")
        cat(sprintf("🔴 航班取消概率：%.3f\n", pred[[1]]))
      })
      
      current_page("result")
    }, error = function(e) {
      print("❌ 出错了：")
      print(e)
      showModal(modalDialog(
        title = "预测失败",
        paste("出错信息：", e$message),
        easyClose = TRUE
      ))
    })
  })
}

shinyApp(ui, server)