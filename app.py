from shiny import App, ui, render, reactive
import shiny
import pandas as pd
import joblib
from datetime import datetime
# from hms import parse as parse_hms
from weather_fetch import get_weather_features_for_user_input
import category_encoders
import xgboost

def parse_hms(timestr):
    try:
        return datetime.strptime(timestr, "%H:%M:%S").time()
    except ValueError:
        return None

# 加载机场信息
airport_info = pd.read_csv("data/airports_info_.csv")

# 查找经纬度函数
def get_coords(iata_code):
    row = airport_info[airport_info['Airport'] == iata_code.upper()]
    if row.empty:
        return (None, None)
    return row.iloc[0]['Latitude'], row.iloc[0]['Longitude']

# 加载模型
model = joblib.load("model/pipe_cancel_weather.pkl")

# UI 定义
app_ui = ui.page_fluid(
    ui.tags.head(
        ui.tags.style("""
        body {
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
        }
        """)
    ),
    ui.div(
        {"class": "box-style"},
        ui.input_text("origin", "Departure Airport*", placeholder="JFK"),
        ui.input_text("dest", "Destination Airport*", placeholder="LAX"),
        ui.input_date("flight_date", "Flight Date*", value=datetime.today()),
        ui.input_text("carrier", "Carrier*", placeholder="AA"),
        ui.input_text("dep_time", "Departure Time*", placeholder="08:00"),
        ui.input_text("arr_time", "Arrival Time*", placeholder="11:30"),
        ui.input_action_button("predict_btn", "🔍 Predict"),
        ui.output_text_verbatim("prediction_output")
    )
)

# 服务器逻辑
def server(input, output, session):
    @reactive.Effect
    @reactive.event(input.predict_btn)
    def predict():
        # 获取用户输入
        origin = input.origin()
        dest = input.dest()
        flight_date = input.flight_date()
        carrier = input.carrier()
        dep_time = input.dep_time()
        arr_time = input.arr_time()

        # 验证输入
        if not all([origin, dest, flight_date, carrier, dep_time, arr_time]):
            output.prediction_output.set("❌ 请填写所有字段。")
            return

        # 获取经纬度
        lat_o, lon_o = get_coords(origin)
        lat_d, lon_d = get_coords(dest)
        if None in [lat_o, lon_o, lat_d, lon_d]:
            output.prediction_output.set("❌ 无法识别输入的机场代码，请检查 IATA 码是否正确。")
            return

        # 获取天气信息
        weather_info = get_weather_features_for_user_input(
            lat_o, lon_o, lat_d, lon_d, flight_date.strftime("%Y-%m-%d")
        )
        if weather_info is None:
            output.prediction_output.set("❌ 无法获取指定日期的天气数据。")
            return

        # 处理时间
        try:
            dep_time_obj = parse_hms(f"{dep_time}:00")
            arr_time_obj = parse_hms(f"{arr_time}:00")
            dep_time_min = dep_time_obj.hour * 60 + dep_time_obj.minute
            arr_time_min = arr_time_obj.hour * 60 + arr_time_obj.minute
            sch_duration = arr_time_min - dep_time_min
            if sch_duration < 0:
                sch_duration += 1440
        except Exception:
            output.prediction_output.set("❌ 时间格式错误，请使用 HH:MM 格式。")
            return


        # 构建输入数据
        input_df = pd.DataFrame([{
            "WEEK": flight_date.isocalendar()[1],
            "MKT_AIRLINE": carrier.upper(),
            "ORIGIN_IATA": origin.upper(),
            "DEST_IATA": dest.upper(),
            "SCH_DEP_TIME": dep_time_min,
            "SCH_ARR_TIME": arr_time_min,
            "SCH_DURATION": sch_duration,
            "DISTANCE": 0,
            "ORIGIN_TYPE": "",
            "ORIGIN_ELEV": 0,
            "DEST_TYPE": "",
            "DEST_ELEV": 0,
            "IS_WEEKEND": flight_date.weekday() >= 5,
            "IS_HOLIDAY": False,
            "DEP_HOUR": dep_time_obj.hour,
            "ARR_HOUR": arr_time_obj.hour,
            **weather_info
        }])

        # 预测
        try:
            pred = model.predict_proba(input_df)[0][1]
            output.prediction_output.set(f"✈️ 预测结果：\n🔴 航班取消概率：{pred:.3f}")
        except Exception as e:
            output.prediction_output.set(f"❌ 预测失败：{str(e)}")

# 创建应用
app = App(app_ui, server)
if __name__ == "__main__":
    shiny.run_app(app)
