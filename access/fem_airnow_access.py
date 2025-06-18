import pyrsig
import numpy as np
import pandas as pd


features_set = [
    'airnow.pm25'
]

download_dates = [
    pd.date_range(start="2023-07-01", end="2023-09-30").to_pydatetime(),
    pd.date_range(start="2023-10-01", end="2023-12-31").to_pydatetime(),
    pd.date_range(start="2024-01-01", end="2024-03-31").to_pydatetime(),
    pd.date_range(start="2024-04-01", end="2024-06-30").to_pydatetime(),
    pd.date_range(start="2024-07-01", end="2024-09-30").to_pydatetime(),
]

bbox = (-95.0, 41.5, -52.0, 63.0)


airNow_payload = []

for dates in download_dates:

    sub_airNow_payload = []

    for date in dates:

        rsigapi = pyrsig.RsigApi(
            bdate = datetime.strftime(date, "%Y-%m-%d"),
            bbox = bbox
            )

        sub2_airNow_payload = []

        for f in features_set:
            df = rsigapi.to_dataframe(f)
            df['pollutant'] = df.columns[-2]
            df.columns = list(df.columns[0:-3]) + ["value"] + list(df.columns[-2:])

            sub2_airNow_payload.append(df)

        sub_airNow_payload.append(pd.concat(sub2_airNow_payload))

    airNow_payload.append(pd.concat(sub_airNow_payload))

    pd.concat(sub_airNow_payload).to_excel("./" + datetime.strftime(date, "%Y-%m-%d") + ".xlsx", index=False)

    if len(airNow_payload) > 0:
        print("completed run for " + datetime.strftime(date, "%Y-%m-%d"))

    time.sleep(20)

