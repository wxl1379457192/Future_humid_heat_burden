import numpy as np
import xarray as xr
import regionmask
import glob
import os
import matplotlib.pyplot as plt
import csv
os.chdir(r"D:\ATtest\Europe_version2")
land_mask = regionmask.defined_regions.natural_earth_v4_1_0
mask = land_mask.land_110
del land_mask
# 设置数据路径
data_folder = 'D:/ATtest/Europe_version2/MPI-ESM1-2-LR/tas'
folder_name= "Result"
os.makedirs(folder_name, exist_ok=True)
csv_filename = os.path.join(folder_name,"global_land_warming_data_areaweighted_annual.csv")
csv_fieldnames = ["Year", "Scenario","Member", "Global_Mean_temp","Global_Mean_Humidex"]
dirlist = ["historical","ssp126","ssp245","ssp370","ssp585"]



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
def Humidexcal(hurs,tas):
    td = tas-((100-hurs)/5)
    e = 6.11*np.exp(5417.7530*(1/273.16-1/(273.16+td)))
    humidex = tas+5/9*(e-10) #
    return (humidex)

with open(csv_filename, mode="w", newline="") as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=csv_fieldnames)
    writer.writeheader()
    for dir in dirlist:
        file_path = glob.glob(os.path.join(data_folder, dir,"*.nc"))
        for filename in file_path:
            nc_file = xr.open_dataset(filename, decode_times=False)
            lon_var = nc_file['lon']
            lat_var = nc_file['lat']
            startyear = filename.split("_")[7][0:4]
            endyear = filename.split("_")[7][7:11]
            var = filename.split("_")[1].split("\\")[2]
            temperature_var = nc_file[var]
            scenario = filename.split("_")[5]

            humname = filename.replace("tas","hurs")
            hum_file = xr.open_dataset(humname, decode_times=False)
            hum_var = hum_file['hurs']

            for year in range(int(startyear),int(endyear)):
                scenario_mean = np.zeros((len(lat_var), len(lon_var)))
                hum_mean = np.zeros((len(lat_var), len(lon_var)))
                for month in range(1, 13):
                    # 查找对应年份和月份的时间索引
                    time_index = (year - int(startyear)) * 12 + month - 1
                    # 读取NC文件中的经纬度坐标
                    lon_var = nc_file['lon']
                    lat_var = nc_file['lat']
                    lon_array = lon_var[:].data
                    lat_array = lat_var[:].data

                    # 循环遍历每个时间步
                    if time_index < len(temperature_var):
                        # # 获取时间信息

                        temperature_masked = temperature_var[time_index, :, :].data
                        temperature_masked[temperature_masked > 350] = np.nan
                        temperature_masked[temperature_masked < -350] = np.nan
                        temperature_masked = temperature_masked - 273.15

                        hum_masked = hum_var[time_index, :, :].data
                        Humidex = Humidexcal(hum_masked,temperature_masked)

                        scenario_mean += temperature_masked
                        hum_mean +=Humidex
                sm = scenario_mean / 12
                hum = hum_mean/12

                area = area_grid(lat_array, lon_array)
                total_area = area.sum(['latitude', 'longitude'])
                # temperature weighted by grid-cell area
                temp_weighted = (sm * area) / total_area
                global_mean = temp_weighted.sum(['latitude', 'longitude'])
                hum_weighted = (hum * area) / total_area
                global_hum = hum_weighted.sum(['latitude', 'longitude'])

                if global_mean == 0:
                    print(filename + " has missing!")
                else:
                    global_mean = global_mean.item()
                    global_hum = global_hum.item()
                    print(str(dir)+" "+str(scenario)+" "+str(year) + " successed: global mean = " + str(global_mean))
                    print(str(dir) + " " + str(scenario) + " " + str(year) + " successed: global humidex mean = " + str(
                        global_hum))

                    # 将数据写入CSV文件
                    writer.writerow({
                        "Year": year,
                        "Scenario": dir,
                        "Member":scenario,
                        "Global_Mean_temp": global_mean,
                        "Global_Mean_Humidex": global_hum,
                    })










