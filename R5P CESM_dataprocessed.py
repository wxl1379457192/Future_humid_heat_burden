import os
import glob
import numpy as np
from osgeo import gdal
from netCDF4 import Dataset
from scipy.interpolate import griddata
import matplotlib.pyplot as plt
os.chdir(r"D:\ATtest\Europe")
import math
import pandas as pd
import numpy as np
# load the dplyr library

def get_study_period(tem_span_file="Result/global_land_warming_data_areaweighted_annual_V2.csv"):
    # read the csv file
    tem_span = pd.read_csv(tem_span_file)
    tem_span = tem_span[tem_span["Global_Mean"] > 0]

    # group by year and scenario and calculate the mean of global mean
    tem_span = tem_span.groupby(["Year", "Scenario"]).agg({"Global_Mean": "mean"}).reset_index()

    # create a span column and a warming column
    tem_span["span"] = 0
    tem_span["warming"] = 0
    tem_span["span"] = tem_span["span"].astype(float)
    tem_span["warming"] = tem_span["warming"].astype(float)
    # loop over the unique scenarios
    for scenario in tem_span["Scenario"].unique():
        scenario_data = tem_span[tem_span["Scenario"] == scenario]

        # loop over the unique years in each scenario and calculate the span value
        for year in scenario_data["Year"].unique():
            tem_span.loc[(tem_span["Year"] == year) & (tem_span["Scenario"] == scenario), "span"] = tem_span.loc[
                (tem_span["Year"] >= year - 10) & (tem_span["Year"] < year + 10) & (tem_span["Scenario"] == scenario),
                "Global_Mean"].mean()

    # calculate the warming value for each scenario
    tem_span["warming"] = tem_span.groupby("Scenario")["span"].transform(
        lambda x: x - x.loc[tem_span["Year"] == 1995].values[0] + 0.6)
    tem_span["warming"] = tem_span["warming"].round(2)

    # filter the rows by the minimum year for each warming value
    span = tem_span.groupby(["Scenario", "warming"]).agg({"Year": "min"}).reset_index()
    span = span[span["Year"]>2023]
    return(span)

def implot(array):
    fig, ax = plt.subplots()
    im = ax.imshow(array, cmap='coolwarm')
    # 设置图表标题和轴标签
    ax.set_title("Mortality number")
    ax.set_xlabel("X axis")
    ax.set_ylabel("Y axis")
    fig.colorbar(im)
    # 显示图表
    plt.show()
# 过滤出所有符合要求的文件名
def writeTiff(im_data, im_geotrans, im_proj, dirpath, filename):
    """
    im_data:2维或3维数据
    dirpath:str或str_array
    """
    if 'int8' in im_data.dtype.name:
        datatype = gdal.GDT_Byte
    elif 'int16' in im_data.dtype.name:
        datatype = gdal.GDT_UInt16
    elif 'int32' in im_data.dtype.name:
        datatype = gdal.GDT_UInt32
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
# 设置数据路径
data_folder = 'E:/D_data/CESM_dataset'
nc_file_pattern_list = ['tas_Amon_CESM1-CAM5_historical_rcp45_{}_*.nc','huss_Amon_CESM1-CAM5_historical_rcp45_{}_*.nc',
                        'tas_Amon_CESM1-CAM5_historical_rcp85_{}_*.nc','huss_Amon_CESM1-CAM5_historical_rcp85_{}_*.nc']
roi_file = 'Humidex_ERA5/Humidex-2022-08-01-00.tif'
outpath = "CESM_amon_new"
if os.path.exists(outpath):
    print(outpath + " has exist")
else:
    os.mkdir(outpath)
dem = "Auxdata/DEM.tif"
DEM_OUT = "Auxdata/DEM_reshape.tif"
#demreshape(dem, 'E:/D_data/CESM_Humidex/Humidex_Amon_CESM1_rcp85_r1i1p1_2025_6.tif',outpath)

# 读取ROI.tif文件获取地理范围
roi_ds = gdal.Open(roi_file)
roi_transform = roi_ds.GetGeoTransform()
roi_resolution = roi_transform[1]
roi_extent = (roi_transform[0], roi_transform[0] + roi_ds.RasterXSize * roi_transform[1],
              roi_transform[3] + roi_ds.RasterYSize * roi_transform[5], roi_transform[3])
span = get_study_period("Result/global_land_warming_data_areaweighted_annual_V2.csv")
span.to_csv("Result/Global_warming.csv", index=False)
yearlist = span["Year"][span["Scenario"] == 'tas_Amon_CESM1-CAM5_historical_rcp45_{}_*.nc'.split("_")[4]].tolist()
yearlist.insert(0, 2022)
humidexout = "E:/D_data/CEMS_Humidex_2023"
if os.path.exists(humidexout):
    print(humidexout+" has existed!")
else:
    os.mkdir(humidexout)
for year in yearlist:
    # warming = str(span["warming"][span["Year"]==year].values[0])
    for month in range(6, 9):
        for nc_file_pattern in nc_file_pattern_list:
            # 创建空数组保存每个月份的温度数据
            temperature_data = np.zeros((roi_ds.RasterYSize, roi_ds.RasterXSize))
            # 创建空列表保存温度数据
            if nc_file_pattern.split("_")[4] == 'rcp45':
                scenario_list = ['r1i1p1', 'r2i1p1', 'r3i1p1', 'r4i1p1', 'r5i1p1',
                                 'r6i1p1', 'r7i1p1', 'r8i1p1', 'r9i1p1', 'r10i1p1',
                                 'r11i1p1', 'r12i1p1', 'r13i1p1', 'r14i1p1', 'r15i1p1']
            else:
                scenario_list = ['r1i1p1', 'r2i1p1', 'r3i1p1', 'r4i1p1', 'r5i1p1',
                                 'r6i1p1', 'r7i1p1', 'r8i1p1', 'r9i1p1', 'r10i1p1',
                                 'r11i1p1', 'r12i1p1', 'r13i1p1', 'r14i1p1', 'r15i1p1',
                                 'r16i1p1', 'r17i1p1', 'r18i1p1', 'r19i1p1', 'r20i1p1',
                                 'r21i1p1', 'r22i1p1', 'r23i1p1', 'r24i1p1', 'r25i1p1',
                                 'r26i1p1', 'r27i1p1', 'r28i1p1', 'r29i1p1', 'r30i1p1',
                                 'r31i1p1', 'r32i1p1', 'r33i1p1', 'r34i1p1', 'r35i1p1',
                                 'r36i1p1', 'r37i1p1', 'r38i1p1', 'r39i1p1', 'r40i1p1']
            # 遍历情景和模拟
            for scenario in scenario_list:  # 根据你的实际情况修改这里的情景和模拟
                filename = nc_file_pattern.split("_")[0] + "_Amon_CESM1_" + nc_file_pattern.split("_")[4] +\
                           "_" + scenario + "_" + str(year) + "_" + str(month) + ".tif"

                if os.path.exists(os.path.join(outpath, filename))|\
                        os.path.exists(os.path.join("E:/D_data/CEMS_Humidex", filename.replace(nc_file_pattern.split("_")[0],"Humidex"))):
                    print(filename + " has existed")
                else:
                    # 构建文件名
                    nc_file_name = nc_file_pattern.format(scenario)
                    nc_path = glob.glob(os.path.join(data_folder, nc_file_name))[0]

                    startyear =nc_path.split("_")[8][0:4] #nc_path.split("_")[7][0:4]
                    # 读取温度数据
                    nc_dataset = Dataset(nc_path)
                    var = nc_file_pattern.split("_")[0]
                    temperature_var = nc_dataset.variables[var]  # 假设温度变量名为 'tas'
                    # 查找对应年份和月份的时间索引
                    time_index = (year - int(startyear)) * 12 + month - 1

                    # 读取NC文件中的经纬度坐标
                    lon_var = nc_dataset.variables['lon']
                    lat_var = nc_dataset.variables['lat']
                    lon_array = lon_var[:].data
                    lat_array = lat_var[:].data

                    # 循环遍历每个时间步
                    if time_index < len(temperature_var):
                        # # 获取时间信息

                        temperature_array = temperature_var[time_index, :, :].data  # 获取温度数据数组
                        #temperature_array = temperature_array - 273.15

                        # 将NC数据的经度坐标数组转换为负值
                        nc_lon_array = lon_array - 360
                        # 将两部分数据进行拼接
                        merged_lon_array = np.concatenate((nc_lon_array, lon_array))
                        merged_temperature_array = np.concatenate((temperature_array, temperature_array), axis=1)

                        # 对数据进行插值以匹配ROI的分辨率
                        xx_nc, yy_nc = np.meshgrid(merged_lon_array, lat_array)

                        # 插值到目标网格
                        xx_roi, yy_roi = np.meshgrid(np.linspace(roi_extent[0], roi_extent[1], roi_ds.RasterXSize),
                                                     np.linspace(roi_extent[2], roi_extent[3], roi_ds.RasterYSize))
                        interpolated_data = griddata((xx_nc.flatten(), yy_nc.flatten()),
                                                     merged_temperature_array.flatten(),
                                                     (xx_roi, yy_roi), method='nearest')
                        interpolated_data = np.flipud(interpolated_data)
                        interpolated_data[interpolated_data>9999] = np.nan
                        interpolated_data = interpolated_data.astype(np.int64)
                        writeTiff(interpolated_data, roi_ds.GetGeoTransform(), roi_ds.GetProjection(), outpath,
                                  filename)
                        # 累加裁剪后的温度数据到对应月份的数组中
                        # temperature_data += interpolated_data
                        print(filename + " successed!")

                        nc_dataset.close()

                    # temperature_data = temperature_data / len(scenario_list)
                    # writeTiff(temperature_data, roi_ds.GetGeoTransform(), roi_ds.GetProjection(), outpath, filename)
                    # print(filename + " successed!")
            #将上述情景中的数据合称为Humidex

        for scenario in ["rcp45", "rcp85"]:
            file_name = 'huss_Amon_CESM1_{}_*.tif'.format(scenario)
            files = glob.glob(os.path.join(outpath, file_name))
            d, geod, projd, wd, hd = openSingleImage(DEM_OUT)
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
                        humidex_pro = (np.nan_to_num(humidex) * 100).astype(np.int16)
                        writeTiff(humidex_pro, geo, proj, humidexout, outname)
                        print(outname + " successed!")
                        if os.path.exists(file):
                            os.remove(file)
                        if os.path.exists(tas):
                            os.remove(tas)






