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
import math
import pandas as pd
import numpy as np
import time
import xarray as xr
import tempfile

def pop_weighted(popnc,vector_data,roi_extent,roi_ds,roi_transform):
    dataset =  xr.open_dataset(popnc)
    subset = dataset.sel(lon=slice(-20, 50), lat=slice(10, 80))
    population_data = subset['Band1'][:].data
    population_data[np.isnan(population_data)] = 0
    lon_var = subset['lon'][:].data
    lat_var = subset['lat'][:].data
    pop  = interpolate(lon_var, lat_var, population_data, roi_extent, roi_ds)
    transform = from_origin(roi_transform[0], roi_transform[3], roi_transform[1],
                            roi_transform[1])
    stats_pop = zonal_stats(vector_data['geometry'], pop, affine=transform,
                            stats="sum")
    pop_sum = [f['sum'] for f in stats_pop]
    p = pd.DataFrame({'pop_sum': pop_sum})
    p = pd.concat([p, vector_data], axis=1)
    p = gpd.GeoDataFrame(p, geometry='geometry')
    p = p.dropna(subset=['pop_sum'])
    geojson_str = p.to_json()

    # 使用ogr.Open读取GeoJSON字符串，创建一个内存中的数据源
    ogr_ds = ogr.Open(geojson_str)
    ogr_lyr = ogr_ds.GetLayer()
    desired_path = 'E:/D_data'
    roi_transform = roi_ds.GetGeoTransform()
    roi_projection = roi_ds.GetProjection()

    pop_raster, pop_raster_path = tempfile.mkstemp(suffix='.tif', dir=desired_path)
    # 使用GDAL创建临时栅格文件
    driver = gdal.GetDriverByName('GTiff')
    out_raster = driver.Create(pop_raster_path, roi_ds.RasterXSize, roi_ds.RasterYSize, 1, gdal.GDT_Float32)

    # 设置新栅格的投影和变换信息
    out_raster.SetProjection(roi_projection)
    out_raster.SetGeoTransform(roi_transform)

    gdal.RasterizeLayer(out_raster, [1], ogr_lyr, options=["ATTRIBUTE=pop_sum"])

    # 从栅格文件中读取数据作为数组
    popsum = out_raster.ReadAsArray()

    # 计算比值
    with np.errstate(divide='ignore', invalid='ignore'):
        weighted = pop / popsum *100000

    weighted[np.isinf(weighted)] = np.nan

    # 创建一个新的.tif文件来保存weighted数组
    weighted_raster_path = popnc.replace(".nc4","_weighted.tif")
    driver = gdal.GetDriverByName('GTiff')
    weighted_raster = driver.Create(weighted_raster_path, roi_ds.RasterXSize, roi_ds.RasterYSize, 1, gdal.GDT_Float64)

    # 设置新栅格的投影和变换信息
    weighted_raster.SetProjection(roi_projection)
    weighted_raster.SetGeoTransform(roi_transform)

    # 将weighted数组写入新栅格文件
    weighted_band = weighted_raster.GetRasterBand(1)
    weighted_band.WriteArray(weighted)
    weighted_band.SetNoDataValue(np.nan)  # 如果您有需要处理的NoData值
    weighted_band.FlushCache()

    # 清理并关闭文件
    weighted_band = None
    weighted_raster = None
    out_raster = None
    os.close(pop_raster)
    os.remove(pop_raster_path)
    print(weighted_raster_path+" has been processed!")
    return weighted_raster_path
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
os.chdir(r"D:\ATtest\Europe_version2")
popdir = "future_pop_data"
shp_file = gpd.read_file('Auxdata/NUTS_level3.shp')
dirlist = ["ssp1", "ssp2", "ssp3", "ssp5","byr"]
roi_file = 'D:/ATtest/Europe/Humidex_ERA5/Humidex-2022-08-01-00.tif'
roi_ds = gdal.Open(roi_file)
band =roi_ds.ReadAsArray()


roi_transform = roi_ds.GetGeoTransform()
roi_extent = (roi_transform[0], roi_transform[0] + roi_ds.RasterXSize * roi_transform[1],
              roi_transform[3] + roi_ds.RasterYSize * roi_transform[5], roi_transform[3])

raster_box = box(*roi_extent)
vector_data = shp_file.to_crs(roi_ds.GetProjection())
#vector_data = vector_data[vector_data.geometry.crosses(raster_box)]
#vector_data.reset_index(drop=True, inplace=True)
#vector_data.plot()

del raster_box

for dir in dirlist:
    popdirname = glob.glob(f'{popdir}/*{dir}*')[0]
    nc_dataset_dir = os.path.join(popdirname, dir.upper() + "_1km")
    popnclist = glob.glob(os.path.join(nc_dataset_dir,'*.nc4'))
    for popnc in popnclist:
        pop_weighted(popnc, vector_data, roi_extent, roi_ds, roi_transform)


