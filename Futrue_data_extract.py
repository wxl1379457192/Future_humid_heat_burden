import concurrent.futures
import os
import glob
import numpy as np
import geopandas as gpd
import rasterio
import zipfile
from osgeo import ogr
from rasterio.features import geometry_mask
from rasterio.io import MemoryFile
from rasterstats import zonal_stats
from osgeo import gdal
from netCDF4 import Dataset
from scipy.interpolate import griddata
import matplotlib.pyplot as plt
import rioxarray
from shapely.geometry import mapping
import gc
try:
    from shapely.geometry import box
except ImportError as e:
    print(f"An error occurred: {e}")
from rasterio.transform import from_origin

os.chdir(r"D:\ATtest\Europe_version2")

import math
import pandas as pd
import numpy as np
import time
import xarray as xr
import tempfile
# load the dplyr library
def get_study_period(tem_span_file="Result/global_land_warming_data_areaweighted_annual.csv"):
    # read the csv file
    tem_span = pd.read_csv(tem_span_file)
    tem_span = tem_span[tem_span["Global_Mean_temp"] > 0]

    # group by year and scenario and calculate the mean of global mean
    tem_span = tem_span.groupby(["Year", "Scenario"]).agg(
        {"Global_Mean_temp": "mean", "Global_Mean_Humidex": "mean"}).reset_index()

    # create a span column and a warming column
    tem_span["span"] = 0
    tem_span["warming"] = 0
    tem_span["span_hum"] = 0
    tem_span["warming_hum"] = 0
    tem_span["span"] = tem_span["span"].astype(float)
    tem_span["warming"] = tem_span["warming"].astype(float)
    tem_span["span_hum"] = tem_span["span_hum"].astype(float)
    tem_span["warming_hum"] = tem_span["warming_hum"].astype(float)

    all = []
    # loop over the unique scenarios
    for scenario in tem_span["Scenario"].unique()[1:]:
        scenario_data = tem_span[(tem_span["Scenario"] == scenario) | (tem_span["Scenario"] == "historical")]

        # loop over the unique years in each scenario and calculate the span value
        for year in scenario_data["Year"].unique():
            scenario_data.loc[(scenario_data["Year"] == year), "span"] = scenario_data.loc[
                (scenario_data["Year"] >= year - 10) & (scenario_data["Year"] < year + 10),
                "Global_Mean_temp"].mean(axis=0)
            scenario_data.loc[(scenario_data["Year"] == year), "span_hum"] = scenario_data.loc[
                (scenario_data["Year"] >= year - 10) & (scenario_data["Year"] < year + 10),
                "Global_Mean_Humidex"].mean(axis=0)
        base_value = scenario_data.loc[scenario_data["Year"] == 1995, "span"].values[0]
        scenario_data.loc[:, "warming"] = scenario_data["span"].transform(lambda x: x - base_value + 0.6)
        hum_value = scenario_data.loc[scenario_data["Year"] == 1995, "span_hum"].values[0]
        scenario_data.loc[:, "warming_hum"] = scenario_data["span_hum"].transform(lambda x: x - hum_value + 0.6)
        all.append(scenario_data)
    # calculate the warming value for each scenario
    tem_span = pd.concat(all, ignore_index=True)
    tem_span["warming"] = tem_span["warming"].round(2)
    tem_span["warming_hum"] = tem_span["warming_hum"].round(2)
    span = tem_span[tem_span["Year"] >= 2022]
    return (span)
def Humidexcal(hurs, tas):
    td = tas - ((100 - hurs) / 5)
    e = 6.11 * np.exp(5417.7530 * (1 / 273.16 - 1 / (273.16 + td)))
    humidex = tas + 5 / 9 * (e - 10)  #
    return (humidex)
def interpolate(lon_array, lat_array, var_array, roi_extent, roi_ds):
    # 将NC数据的经度坐标数组转换为负值
    nc_lon_array = lon_array - 360
    # 将两部分数据进行拼接
    merged_lon_array = np.concatenate((nc_lon_array, lon_array))
    merged_temperature_array = np.concatenate((var_array, var_array), axis=1)

    # 对数据进行插值以匹配ROI的分辨率
    xx_nc, yy_nc = np.meshgrid(merged_lon_array, lat_array)

    # 插值到目标网格
    xx_roi, yy_roi = np.meshgrid(np.linspace(roi_extent[0], roi_extent[1], roi_ds.RasterXSize),
                                 np.linspace(roi_extent[2], roi_extent[3], roi_ds.RasterYSize))
    interpolated_data = griddata((xx_nc.flatten(), yy_nc.flatten()),
                                 merged_temperature_array.flatten(),
                                 (xx_roi, yy_roi), method='nearest')
    interpolated_data = np.flipud(interpolated_data)
    interpolated_data[interpolated_data > 9999] = np.nan
    return interpolated_data
def round_down_year(year):
    return (year // 10) * 10

def process_file(file,roi_file,shp_file, humidexout, outname, nc_dataset_dir):
    # 读取ROI.tif文件获取地理范围
    roi_ds = gdal.Open(roi_file)
    roi_transform = roi_ds.GetGeoTransform()
    roi_extent = (roi_transform[0], roi_transform[0] + roi_ds.RasterXSize * roi_transform[1],
                  roi_transform[3] + roi_ds.RasterYSize * roi_transform[5], roi_transform[3])

    raster_box = box(*roi_extent)
    vector_data = shp_file.to_crs(roi_ds.GetProjection())
    #vector_data = vector_data[vector_data.geometry.intersects(raster_box)]
    #vector_data.reset_index(drop=True, inplace=True)
    del raster_box
    span = get_study_period("Result/global_land_warming_data_areaweighted_annual.csv")
    span.to_csv("Result/Global_warming.csv", index=False)
    yearlist = span["Year"].unique()

    all = pd.DataFrame()
    output_csv_path = os.path.join(humidexout,outname + "_" + file.split("_")[4]+"_"+ file.split("_")[5] + ".csv")
    if os.path.exists(output_csv_path):
        print(output_csv_path + " has existed")
    else:

        hursname = file.replace("tas", "hurs")
        startyear = file.split("_")[7][0:4]

        # 读取温度数据
        nc_dataset = Dataset(file)
        #nc_dataset = nc_dataset.sel(lon=slice(bounds[0], bounds[2]), lat=slice(bounds[1], bounds[3]))
        temperature_var = nc_dataset.variables['tas']  # 假设温度变量名为 'tas'
        hurs_dataset = Dataset(hursname)
        hurs_var = hurs_dataset.variables['hurs']  # 假设温度变量名为 'tas'
        # 读取NC文件中的经纬度坐标
        lon_var = nc_dataset.variables['lon']
        lat_var = nc_dataset.variables['lat']
        lon_array = lon_var[:].data
        lat_array = lat_var[:].data

        del nc_dataset
        for year in yearlist:
            roundyear = round_down_year(year)
            popnc =  glob.glob(f'{nc_dataset_dir}/*{roundyear}*')[0].replace(".nc4","_weighted.tif")
            d = gdal.Open(popnc,gdal.GA_ReadOnly)
            weightedpop = d.ReadAsArray()
            weightedpop = weightedpop/100000
            weightedpop[np.isnan(weightedpop)] = 0
            weightedpop[np.isinf(weightedpop)] = 0

            # 记录开始时间
            start_time = time.time()
            for month in range(6, 9):

                # 查找对应年份和月份的时间索引
                time_index = (year - int(startyear)) * 12 + month - 1
                # 循环遍历每个时间步
                if time_index < len(temperature_var):
                    # # 获取时间信息
                    temperature_array = temperature_var[time_index, :, :].data  # 获取温度数据数组
                    temperature_array = temperature_array - 273.15
                    hurs_array = hurs_var[time_index, :, :].data
                    humidex_array = Humidexcal(hurs_array, temperature_array)
                    tas = interpolate(lon_array, lat_array, temperature_array, roi_extent, roi_ds)
                    humidex = interpolate(lon_array, lat_array, humidex_array, roi_extent, roi_ds)
                    tas = tas*weightedpop
                    humidex = humidex*weightedpop
                    del temperature_array, humidex_array
                    transform = from_origin(roi_transform[0], roi_transform[3], roi_transform[1],
                                            roi_transform[1])
                    stats_tas = zonal_stats(vector_data['geometry'], tas, affine=transform,
                                            stats="sum")
                    stats_hum = zonal_stats(vector_data['geometry'], humidex, affine=transform,
                                            stats="sum")
                    Tas_mean = [f['sum'] for f in stats_tas]
                    Humidex_mean = [f['sum'] for f in stats_hum]

                    del tas, humidex
                    df = pd.DataFrame({'Humidex_mean': Humidex_mean, 'Tas_mean': Tas_mean})

                    df = pd.concat([df, vector_data], axis=1)
                    df = df.dropna(subset=['Humidex_mean'])
                    df['year'] = year
                    df['month'] = month
                    del stats_hum, Humidex_mean

                    # 记录结束时间
                    end_time = time.time()
                    # 计算时间差，得到代码执行所花费的时间
                    execution_time = end_time - start_time
                    print(output_csv_path, year, "-", month, "执行时间：", execution_time, "秒")

                    all = pd.concat([all, df], axis=0)

                    del df
                    gc.collect()

        all.to_csv(output_csv_path, index=False)
# 主函数
def main():
    shp_file = gpd.read_file('Auxdata/NUTS_level3.shp')
    # 设置数据路径
    data_folder = 'D:/ATtest/Europe_version2/MPI-ESM1-2-LR/tas'
    dirlist = ["ssp126", "ssp245", "ssp370", "ssp585"]
    popdir = "future_pop_data"

    roi_file = 'D:/ATtest/Europe/Humidex_ERA5/Humidex-2022-08-01-00.tif'

    humidexout = "E:/D_data/CEMS_Humidex_pop"
    if not os.path.exists(humidexout):
        os.mkdir(humidexout)
    outname = "Hourly_future"

    # 使用ProcessPoolExecutor来创建一个进程池
    with concurrent.futures.ProcessPoolExecutor(max_workers=12) as executor:
        for dir in dirlist:
            file_path = glob.glob(os.path.join(data_folder, dir, "*.nc"))
            popdirname = glob.glob(f'{popdir}/*{dir[:4]}*')[0]

            nc_dataset_dir = os.path.join(popdirname,dir[:4].upper()+"_1km")
            for file in file_path:
                #process_file(file,roi_file,shp_file, humidexout, outname, nc_dataset_dir)
                #将任务提交到进程池
               executor.submit(process_file, file,roi_file,shp_file, humidexout, outname, nc_dataset_dir)

if __name__ == "__main__":
    main()









