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

def tif2csv(indir,outpath,outname,scenario):
    # 保存DataFrame为CSV文件
    output_csv_path = os.path.join(outpath, outname + "_" + scenario + ".csv")
    if not os.path.exists(output_csv_path):
        pattern = ".*." + scenario + ".*\.tif"
        files_list = [f for f in os.listdir(indir) if re.match(pattern, f)]
        all = pd.DataFrame()

        for file in files_list:
            print(file)
            # 记录开始时间
            start_time = time.time()

            raster_path = os.path.join(indir, file)
            raster_data = rasterio.open(raster_path)
            raster_data_data = raster_data.read(1)
            raster_data_data = raster_data_data / 100
            raster_data_data[raster_data_data == 0] = np.nan
            # 读取矢量面文件
            vector_path = 'NUTS_RG_20M_2021_4326/NUTS_LEVEL3.shp'
            vector_data = gpd.read_file(vector_path)
            vector_data = vector_data.to_crs(raster_data.crs)

            transform = from_origin(raster_data.bounds.left, raster_data.bounds.top, raster_data.res[0],
                                    raster_data.res[1])
            stats_hum = zonal_stats(vector_data['geometry'], raster_data_data, affine=transform, stats="mean")
            Humidex_mean = [f['mean'] for f in stats_hum]
            del raster_data
            df = pd.DataFrame(Humidex_mean)
            df = df.rename(columns={0: 'Humidex_mean'})
            df = pd.concat([df, vector_data], axis=1)
            df['year'] = file.split("_")[5]
            df['month'] = file.split("_")[6][0]
            # 记录结束时间
            end_time = time.time()
            # 计算时间差，得到代码执行所花费的时间
            execution_time = end_time - start_time
            print(file, "执行时间：", execution_time, "秒")
            del stats_hum, Humidex_mean
            all = pd.concat([all, df], axis=0)
            del df

        all.to_csv(output_csv_path, index=False)
        print(raster_path + " has been processed")
    else:
        print(output_csv_path + " has existed")
if __name__ == "__main__":
    pd.options.mode.chained_assignment = None
    data_path = "E:/D_data/CESM_Humidex_2023/"
    outdir = "Future_data_CESM_2023"
    outname = "Hourly_CESM_future_2023"
    scenarios = ["rcp45_r1i1p1",  "rcp45_r3i1p1", "rcp45_r4i1p1", "rcp45_r5i1p1",
                 "rcp45_r6i1p1", "rcp45_r7i1p1", "rcp45_r8i1p1", "rcp45_r9i1p1", "rcp45_r10i1p1",
                 "rcp45_r11i1p1", "rcp45_r12i1p1", "rcp45_r13i1p1", "rcp45_r14i1p1", "rcp45_r15i1p1",
                 "rcp85_r1i1p1", "rcp85_r2i1p1", "rcp85_r3i1p1", "rcp85_r4i1p1", "rcp85_r5i1p1",
                 "rcp85_r6i1p1", "rcp85_r7i1p1", "rcp85_r8i1p1", "rcp85_r9i1p1","rcp85_r10i1p1",
                 "rcp85_r11i1p1", "rcp85_r12i1p1", "rcp85_r13i1p1", "rcp85_r14i1p1", "rcp85_r15i1p1",
                 "rcp85_r16i1p1", "rcp85_r17i1p1","rcp85_r18i1p1", "rcp85_r19i1p1", "rcp85_r20i1p1",
                 "rcp85_r21i1p1", "rcp85_r22i1p1", "rcp85_r23i1p1", "rcp85_r24i1p1", "rcp85_r25i1p1",
                 "rcp85_r26i1p1", "rcp85_r27i1p1", "rcp85_r28i1p1", "rcp85_r29i1p1", "rcp85_r30i1p1",
                 "rcp85_r31i1p1", "rcp85_r32i1p1", "rcp85_r33i1p1", "rcp85_r34i1p1", "rcp85_r35i1p1",
                 "rcp85_r36i1p1", "rcp85_r37i1p1", "rcp85_r38i1p1", "rcp85_r39i1p1", "rcp85_r40i1p1"]

    if (os.path.exists(outdir)):
        print(outdir + " has existed!!!")
    else:
        os.mkdir(outdir)

    for scenario in scenarios:
        tif2csv(data_path,outdir,outname,scenario)

