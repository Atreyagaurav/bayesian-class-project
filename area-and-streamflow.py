import sys
import pandas as pd

try:
    station = sys.argv[1]
except IndexError:
    print("Give station")
    exit(1)


print("nadi:var:sf_mean=", float(sf.mean()))

for year, flow in sf.groupby(sf.index.year).mean().items():
    print(f"nadi:var:sf_year_{year}={flow}")

for month, flow in sf.groupby(sf.index.month).mean().items():
    print(f"nadi:var:sf_month_{month}={flow}")

exit(0)


nodes = [
    "03219500", "03220000", "03221000", "03223425", "03225500",
    "03226800", "03227500", "03228300", "03228500", "03228750",
    "03228805", "03229500", "03229610",
]


means = {}
for station in nodes:
    df = pd.read_csv(f"data/streamflow/{station}.csv", header=None)
    sf = df.iloc[:, 4]
    sf.index = pd.to_datetime(df.iloc[:, 2])
    daily = sf.resample('1d').mean()
    counts = daily.groupby(daily.index.year).count()
    counts.index.name = "datetime"
    daily.index.name = "datetime"
    m = daily.groupby(daily.index.year).mean().loc[counts > 300]
    m.name = station
    m.index.name = "year"
    means[station] = m


yearly_mean = pd.DataFrame(means, index=range(2005, 2025))

ind = pd.read_csv("node-attrs.csv", index_col="name", dtype={"name": str}).INDEX
area = pd.read_csv("node-attrs.csv", index_col="name", dtype={"name": str}).area
data = yearly_mean.T # .dropna()
data.columns = [f"y_{y}" for y in data.columns]
data.loc[:, "area"] = [area.loc[i] for i in  data.index]
data = data.join(ind).sort_values("INDEX")

# for _, row in data.iterrows():
#     i = row.INDEX
#     print(f"\\Node[0]{{{i}}}{{{i}}} & {area.loc[row.name]:.2f} & {row.y_2005:.2f} & \\(\\hdots\\) & {row.y_2023} & {row.y_2024} \\\\[2mm]")



data.to_csv("annual-means-gaps.csv")
long_df = pd.DataFrame(yearly_mean.stack())
long_df.columns = ["streamflow"]
long_df.index.names = ("year", "station")
long_df.loc[:, "node"] = [ind.loc[i] for _,i in  long_df.index]
long_df.loc[:, "area"] = [area.loc[i] for _,i in  long_df.index]

long_df.to_csv("annual-means-long.csv")

