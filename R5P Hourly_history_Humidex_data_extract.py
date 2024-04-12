import os
import geopandas as gpd
import rasterio
from rasterio import features
import numpy as np
import fiona
from shapely.geometry import shape
from rasterstats import zonal_stats
import time
import pandas as pd
import re
import shutil
from rasterio.transform import from_origin
os.chdir(r"D:\ATtest\Europe")

def tif2csv(indir,outpath,outname):
    pattern = "Humidex.*\.tif"
    files_list = [f for f in os.listdir(indir) if re.match(pattern, f)]
    all = pd.DataFrame()
    for file in files_list:
        # 记录开始时间
        start_time = time.time()

        raster_path = os.path.join(indir,file)
        raster_data = rasterio.open(raster_path)
        raster_data_data = raster_data.read(1)
        raster_data_data = raster_data_data / 100
        raster_data_data[raster_data_data == 0] = np.nan
        # 读取矢量面文件
        vector_path = 'NUTS_RG_20M_2021_4326/NUTS_LEVEL3.shp'
        vector_data = gpd.read_file(vector_path)
        vector_data = vector_data.to_crs(raster_data.crs)
        del raster_data
        transform = from_origin(raster_data.bounds.left, raster_data.bounds.top, raster_data.res[0], raster_data.res[1])
        stats_hum = zonal_stats(vector_data['geometry'], raster_data_data, affine=transform, stats="mean")
        Humidex_mean = [f['mean'] for f in stats_hum]

        df = pd.DataFrame(Humidex_mean)
        df = df.rename(columns={0: 'Humidex_mean'})
        df = pd.concat([df, vector_data], axis=1)
        df['Date'] = file.split("Humidex-")[1][0:10]
        df['hour'] = file.split("Humidex-")[1][11:13]
        df = df.dropna(subset=['Humidex_mean'])
        # 记录结束时间
        end_time = time.time()
        # 计算时间差，得到代码执行所花费的时间
        execution_time = end_time - start_time
        print(file,"执行时间：", execution_time, "秒")
        del stats_hum, Humidex_mean
        all = pd.concat([all,df], axis=0)
        del df

    # 保存DataFrame为CSV文件
    output_csv_path = os.path.join(outpath,outname+".csv")
    all.to_csv(output_csv_path, index=False)
    print(raster_path+" has been processed")

if __name__ == "__main__":
    pd.options.mode.chained_assignment = None
    data_path = "Humidex_ERA5/"
    outdir = "Hourly_data_2022"
    outname = "Hourly_Humidex_2022"


    if (os.path.exists(outdir)):
        print(outdir + " has existed!!!")
    else:
        os.mkdir(outdir)


    tif2csv(data_path,outdir,outname)