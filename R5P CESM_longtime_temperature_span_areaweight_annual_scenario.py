import numpy as np
import xarray as xr
import regionmask
import glob
import os
import matplotlib.pyplot as plt
import csv
os.chdir(r"D:\ATtest\Europe")
land_mask = regionmask.defined_regions.natural_earth_v4_1_0
mask = land_mask.land_110
del land_mask
# 设置数据路径
data_folder = 'E:/D_data/CESM_dataset'
nc_file_pattern_list = ['tas_Amon_CESM1-CAM5_historical_rcp45_{}_*.nc','tas_Amon_CESM1-CAM5_historical_rcp85_{}_*.nc']
csv_filename = "Result/global_land_warming_data_areaweighted_annual.csv"
csv_fieldnames = ["Year", "Scenario","Member", "Global_Mean"]


def earth_radius(lat):
    '''
    calculate radius of Earth assuming oblate spheroid
    defined by WGS84

    Input
    ---------
    lat: vector or latitudes in degrees

    Output
    ----------
    r: vector of radius in meters

    Notes
    -----------
    WGS84: https://earth-info.nga.mil/GandG/publications/tr8350.2/tr8350.2-a/Chapter%203.pdf
    '''
    from numpy import deg2rad, sin, cos

    # define oblate spheroid from WGS84
    a = 6378137
    b = 6356752.3142
    e2 = 1 - (b ** 2 / a ** 2)

    # convert from geodecic to geocentric
    # see equation 3-110 in WGS84
    lat = deg2rad(lat)
    lat_gc = np.arctan((1 - e2) * np.tan(lat))

    # radius equation
    # see equation 3-107 in WGS84
    r = (
            (a * (1 - e2) ** 0.5)
            / (1 - (e2 * np.cos(lat_gc) ** 2)) ** 0.5
    )

    return r
def area_grid(lat, lon):
        """
        Calculate the area of each grid cell
        Area is in square meters

        Input
        -----------
        lat: vector of latitude in degrees
        lon: vector of longitude in degrees

        Output
        -----------
        area: grid-cell area in square-meters with dimensions, [lat,lon]

        Notes
        -----------
        Based on the function in
        https://github.com/chadagreene/CDT/blob/master/cdt/cdtarea.m
        """
        from numpy import meshgrid, deg2rad, gradient, cos
        from xarray import DataArray

        xlon, ylat = meshgrid(lon, lat)
        R = earth_radius(ylat)

        dlat = deg2rad(gradient(ylat, axis=0))
        dlon = deg2rad(gradient(xlon, axis=1))

        dy = dlat * R
        dx = dlon * R * cos(deg2rad(ylat))

        area = dy * dx

        xda = DataArray(
            area,
            dims=["latitude", "longitude"],
            coords={"latitude": lat, "longitude": lon},
            attrs={
                "long_name": "area_per_pixel",
                "description": "area per pixel",
                "units": "m^2",
            },
        )
        return xda

with open(csv_filename, mode="w", newline="") as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=csv_fieldnames)
    writer.writeheader()
    for nc_file_pattern in nc_file_pattern_list:
        for year in range(1960, 2101, 1):
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

            for scenario in scenario_list:  # 根据你的实际情况修改这里的情景和模拟
                # 创建空数组保存每个月份的温度数据
                # 创建空列表保存温度数据
                # 遍历情景和模拟
                filename = str(year) + "_" + str(1) + ".tif"
                file_name = nc_file_pattern.format(scenario_list[0])
                file_path = glob.glob(os.path.join(data_folder, file_name))[0]
                # 打开 NetCDF 文件
                nc_file = xr.open_dataset(file_path)
                lon_var = nc_file['lon']
                lat_var = nc_file['lat']
                scenario_mean = np.zeros((len(lat_var), len(lon_var)))
                for month in range(1, 13):
                    # temperature_data = np.zeros((len(lat_var), len(lon_var)))
                    nc_file_name = nc_file_pattern.format(scenario)
                    nc_path = glob.glob(os.path.join(data_folder, nc_file_name))[0]

                    startyear = nc_path.split("_")[8][0:4]
                    # 读取温度数据
                    nc_dataset = xr.open_dataset(nc_path, decode_times=False)
                    var = nc_file_pattern.split("_")[0]
                    temperature_var = nc_dataset[var]  # 假设温度变量名为 'tas'

                    # 使用地区掩膜对 NetCDF 数据进行掩膜
                    # water = temperature_var.where(mask.mask)
                    # water = water[1,:,:].data
                    # water[np.isnan(water)] = 1
                    # water[water>1] = 0

                    # 查找对应年份和月份的时间索引
                    time_index = (year - int(startyear)) * 12 + month - 1

                    # 读取NC文件中的经纬度坐标
                    lon_var = nc_dataset['lon']
                    lat_var = nc_dataset['lat']
                    lon_array = lon_var[:].data
                    lat_array = lat_var[:].data

                    # 循环遍历每个时间步
                    if time_index < len(temperature_var):
                        # # 获取时间信息

                        temperature_masked = temperature_var[time_index, :, :].data
                        temperature_masked[temperature_masked > 350] = np.nan
                        temperature_masked[temperature_masked < -350] = np.nan
                        # temperature_masked[water==0] =np.nan
                        temperature_masked = temperature_masked - 273.15

                        scenario_mean += temperature_masked
                        nc_dataset.close()
                # print(str(month)+" has added.")

                sm = scenario_mean / 12

                area = area_grid(lat_array, lon_array)
                total_area = area.sum(['latitude', 'longitude'])
                # temperature weighted by grid-cell area
                temp_weighted = (sm * area) / total_area
                global_mean = temp_weighted.sum(['latitude', 'longitude'])

                if global_mean == 0:
                    print(filename + " has missing!")
                else:
                    global_mean = global_mean.item()

                    ##area[np.isnan(temperature_masked)] = np.nan
                    # area_weighted = area / np.nansum(area)
                    # global_mean = np.nansum(scenario_mean)
                    # global_mean = np.nanmean(scenario_mean)

                    print(str(scenario)+str(year) + " successed: global mean = " + str(global_mean))
                    var = nc_file_pattern.split("_")[4]

                    # 将数据写入CSV文件
                    writer.writerow({
                        "Year": year,
                        "Scenario": var,
                        "Member":scenario,
                        "Global_Mean": global_mean
                    })












