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
from osgeo import gdal
from rasterio.transform import from_origin
from netCDF4 import Dataset, num2date
from osgeo import ogr
os.chdir(r"D:\ATtest\Europe_version2")
def tif2csv(popfile,hourlyfile,outpath,outname,roi_file):
    d = gdal.Open(popfile, gdal.GA_ReadOnly)

    roi_ds = gdal.Open(roi_file)
    roi_transform = roi_ds.GetGeoTransform()
    vector_path = 'Auxdata/NUTS_level3.shp'
    vector_data = gpd.read_file(vector_path)
    vector_data = vector_data.to_crs(roi_ds.GetProjection())

    nc_dataset = Dataset(hourlyfile)

    bandnum = nc_dataset.variables['variable'].shape[0]
    all = pd.DataFrame()
    humidexdata = nc_dataset.variables['variable']

    weightedpop = d.ReadAsArray()
    weightedpop = weightedpop[:,3:weightedpop.shape[1]-3]
    weightedpop[np.isnan(weightedpop)] = 0
    weightedpop[np.isinf(weightedpop)] = 0
    timelist = nc_dataset.variables['time']
    # 读取time变量的数据
    times =timelist[:]

    # 将时间单位转换为日期时间对象
    dates = num2date(times, units=timelist.units)
    for filenum in range(0,bandnum):
        # 记录开始时间
        start_time = time.time()

        raster_data = humidexdata[filenum, :, :].data

        raster_data = raster_data * weightedpop/100000

        transform = from_origin(roi_transform[0], roi_transform[3], roi_transform[1],
                                roi_transform[1])
        stats_hum = zonal_stats(vector_data['geometry'], raster_data, affine=transform, stats="sum")
        Humidex_mean = [f['sum'] for f in stats_hum]

        df = pd.DataFrame(Humidex_mean)
        df = df.rename(columns={0: 'Humidex_mean'})
        df = pd.concat([df, vector_data], axis=1)

        date = dates[filenum]
        df['Date'] =date.strftime('%Y-%m-%d')
        df['hour'] = date.strftime('%H')
        df = df.dropna(subset=['Humidex_mean'])
        # 记录结束时间
        end_time = time.time()
        # 计算时间差，得到代码执行所花费的时间
        execution_time = end_time - start_time
        print(filenum,"执行时间：", execution_time, "秒")
        del stats_hum, Humidex_mean
        all = pd.concat([all,df], axis=0)
        del df

    # 保存DataFrame为CSV文件
    output_csv_path = os.path.join(outpath,outname+".csv")
    all.to_csv(output_csv_path, index=False)
    #print(raster_path+" has been processed")

if __name__ == "__main__":
    pd.options.mode.chained_assignment = None
    hourlyfile = "E:/D_data/Predict_humidex.nc"
    outdir = "Hourly_data_2022"
    outname = "Hourly_Humidex_2022"
    popfile = "future_pop_data/popdynamics-1-km-downscaled-pop-base-year-projection-ssp-2000-2100-rev01-byr-netcdf/BYR_1km/baseYr_total_2000_weighted.tif"
    roi_file =  'D:/ATtest/Europe/Humidex_ERA5/Humidex-2022-08-01-00.tif'
    if (os.path.exists(outdir)):
        print(outdir + " has existed!!!")
    else:
        os.mkdir(outdir)


    tif2csv(popfile,hourlyfile,outdir,outname,roi_file)