def calculatePressure(altitude,tas):
    #标准大气压模型的参数
    P0 = 1013.25 # the pressure at the reference level (hPa)
    g = 9.80665  # the acceleration due to the gravitational force(m/s2)
    m = 0.0289644 #  the molar mass of air (kg/mol)
    r = 8.31432 #  the universal gas constant (J/(mol·K))

    p = P0 * math.e ** (( -g * m * altitude )/ (r * tas))

    # 将气压转换为帕斯卡（Pa）
    #pressure = p * 100
    return(p)
def Humidexcal(huss,tas,altitude):
    p = calculatePressure(altitude,tas)
    e = (p*huss)/(0.622+0.378*huss)
    humidex = tas+5/9*(e-10) #
    return(humidex)
def openSingleImage(imagefilename):
    """读取影像文件"""
    dataset = gdal.Open(imagefilename)
    im_width = dataset.RasterXSize  # 列数
    im_height = dataset.RasterYSize  # 行数
    im_bands = dataset.RasterCount  # 波段数
    im_geotrans = dataset.GetGeoTransform()  # 仿射矩阵
    # 共有六个参数，分别代表分表代表左上角x坐标；东西方向上图像的分辨率；如果北边朝上，地图的旋转角度，0表示图像的行与x轴平行；左上角y坐标；
    # 如果北边朝上，地图的旋转角度，0表示图像的列与y轴平行；南北方向上地图的分辨率。
    im_proj = dataset.GetProjection()  # 地图投影信息
    im_band = dataset.GetRasterBand(1)
    Image = im_band.ReadAsArray(0, 0, im_width, im_height)
    del dataset
    # 关闭图像进程
    return np.double(Image), im_geotrans, im_proj, im_width, im_height
def demreshape(dem_path, tagerfile_path,output_path):

    # 打开DEM数据
    dem_dataset = gdal.Open(dem_path, gdal.GA_ReadOnly)

    # 获取DEM的投影信息
    dem_projection = dem_dataset.GetProjection()

    # 打开TAS数据
    tag_dataset = gdal.Open(tagerfile_path, gdal.GA_ReadOnly)

    # 获取TAS的地理变换信息
    tag_transform = tag_dataset.GetGeoTransform()

    # 获取TAS的投影信息
    tag_projection = tag_dataset.GetProjection()

    # 获取TAS的行列数
    tag_cols = tag_dataset.RasterXSize
    tag_rows = tag_dataset.RasterYSize

    # 创建输出数据集
    driver = gdal.GetDriverByName('GTiff')
    output_dataset = driver.Create(output_path, tag_cols, tag_rows, 1, gdal.GDT_Float32)

    # 设置输出数据集的地理变换信息
    output_dataset.SetGeoTransform(tag_transform)

    # 设置输出数据集的投影信息
    output_dataset.SetProjection(tag_projection)

    # 执行DEM数据转换（使用均值重采样）
    gdal.ReprojectImage(dem_dataset, output_dataset, dem_projection, tag_projection, gdal.GRA_Average)

    # 关闭数据集
    dem_dataset = None
    tag_dataset = None
    output_dataset = None

    print("DEM数据转换完成")
def writeTiff(im_data, im_geotrans, im_proj, dirpath, filename):
    """
    im_data:2维或3维数据
    dirpath:str或str_array
    """
    if 'int8' in im_data.dtype.name:
        datatype = gdal.GDT_Byte
    elif 'int16' in im_data.dtype.name:
        datatype = gdal.GDT_UInt16
    else:
        datatype = gdal.GDT_Float32
    number = 1
    im_bands = 1  # 均为单波段影像
    fullpath = []
    newpath = []
    newim_data = []
    newfilename = []
    ndv = np.nan
    if isinstance(filename, str):
        fullpath = dirpath + "\\" + filename
    else:
        for i in range(len(filename)):
            fullpath.append(dirpath + "\\" + filename[i])
    if len(im_data.shape) == 3:
        number, im_height, im_width = im_data.shape
        newpath = fullpath
        newim_data = im_data
        newfilename = filename
    elif len(im_data.shape) == 2:
        im_height, im_width = im_data.shape
        newpath.append(fullpath)
        newim_data.append(im_data)
        newfilename.append(filename)
    else:
        number, (im_height, im_width) = 1, im_data.shape
        print("Error:There is no way to get here!!!!")
    for i in range(number):
        # 创建文件
        driver = gdal.GetDriverByName("GTiff")
        dataset = driver.Create(newpath[i], im_width, im_height, im_bands, datatype)
        if (dataset != None):
            dataset.SetGeoTransform(im_geotrans)  # 写入仿射变换参数
            dataset.SetProjection(im_proj)  # 写入投影
            dataset.GetRasterBand(1).SetNoDataValue(ndv)  # 设置nodata值
        for band in range(im_bands):
            dataset.GetRasterBand(band + 1).WriteArray(newim_data[i])
        del dataset
import math
import os
import glob
import numpy as np
from osgeo import gdal
import shutil
import matplotlib.pyplot as plt
import time
os.chdir(r"D:\ATtest\Europe")
data_folder = "CESM_amon_new"
file_pattern = 'huss_Amon_CESM1_{}_*.tif'
scenarios = ["rcp45","rcp85"]
dem = "Auxdata/DEM.tif"
outpath = "Auxdata/DEM_reshape.tif"
demreshape(dem, 'E:/D_data/CESM_Humidex/Humidex_Amon_CESM1_rcp85_r1i1p1_2025_6.tif',outpath)
humidexout = "E:/D_data/CESM_Humidex_2023"
if os.path.exists(humidexout):
    print(humidexout+" has existed!")
else:
    os.mkdir(humidexout)
for i in range(0,10):
    for scenario in scenarios:
        file_name = file_pattern.format(scenario)
        files = glob.glob(os.path.join(data_folder, file_name))
        d, geod, projd, wd, hd = openSingleImage(outpath)
        for file in files:
            tas = file.replace("huss", "tas")
            if os.path.exists(file):
                outname = file.split("\\")[1].replace("huss", "Humidex")
                if os.path.exists(os.path.join(humidexout, outname)):
                    if os.path.exists(file):
                        os.remove(file)
                    if os.path.exists(tas):
                        os.remove(tas)
                    print(outname + " has existed!")
                else:
                    t, geo, proj, w, h = openSingleImage(tas)
                    h, _, _, _, _ = openSingleImage(file)
                    t = t - 273.15
                    humidex = Humidexcal(h, t, d)
                    humidex[humidex > 500] = np.nan
                    writeTiff(humidex, geo, proj, humidexout, outname)
                    print(outname + " successed!")

                    if os.path.exists(file):
                        os.remove(file)
                    if os.path.exists(tas):
                        os.remove(tas)

    time.sleep(0)





#shutil.rmtree(data_folder)




