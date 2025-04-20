import sys
import pandas as pd

try:
    station = sys.argv[1]
except IndexError:
    print("Give station")
    exit(1)

df = pd.read_csv(f"data/streamflow/{station}.csv", header=None)
sf = df.iloc[:, 4]
sf.index = pd.to_datetime(df.iloc[:, 2])
print("nadi:var:sf_mean=", float(sf.mean()))

for year, flow in sf.groupby(sf.index.year).mean().items():
    print(f"nadi:var:sf_year_{year}={flow}")

for month, flow in sf.groupby(sf.index.month).mean().items():
    print(f"nadi:var:sf_month_{month}={flow}")
