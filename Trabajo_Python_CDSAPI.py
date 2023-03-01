# -*- coding: utf-8 -*-
"""
Created on Mon Nov 15 10:51:27 2021
@author: arw
"""
import cdsapi
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import matplotlib.pyplot as plt
import numpy as np
from netCDF4 import Dataset, num2date
from cartopy.util import add_cyclic_point
"""
#------------------------------------------------------------------>
c = cdsapi.Client()
c.retrieve('cams-global-atmospheric-composition-forecasts',
{
'date': '2021-11-15/2021-11-18',
'type': 'forecast',
'format': 'netcdf_zip',
'variable': ['dust_aerosol_optical_depth_550nm',
'nitrogen_dioxide',
'ozone',
'particulate_matter_10um',
'particulate_matter_2.5um',
'sulphur_dioxide'],
'presure_level':['925','950','1000',],
'time': '00:00',
'leadtime_hour': list(range(0,90,6)),
'area': [20,-100,0,-40],
},
'predicciones.zip')
#------------------------------------------------------------------>
"""
#------------------------------------------------------------------>
c = cdsapi.Client()
c.retrieve(
'cams-global-atmospheric-composition-forecasts',
{
'type': 'forecast',
'format': 'netcdf_zip',
'variable': [
'dust_aerosol_optical_depth_550nm', 'nitrogen_dioxide', 'ozone',
'particulate_matter_10um', 'particulate_matter_2.5um', 'sulphur_dioxide',
],
'pressure_level': [
'925', '950', '1000',
],
'leadtime_hour': [
'0', '1', '10',
'11', '12', '13',
'14', '15', '16',
'17', '18', '19',
'2', '20', '21',
'22', '23', '24',
'25', '26', '27',
'28', '29', '3',
'30', '31', '32',
'33', '34', '35',
'36', '37', '38',
'39', '4', '40',
'41', '42', '43',
'44', '45', '46',
'47', '48', '49',
'5', '50', '51',
'52', '53', '54',
'55', '56', '57',
'58', '59', '6',
'60', '61', '62',
'63', '64', '65',
'66', '67', '68',
'69', '7', '70',
'71', '72', '73',
'74', '75', '76',
'77', '78', '79',
'8', '80', '81',
'82', '83', '84',
'85', '86', '87',
'88', '89', '9',
'90',
],
'time': '00:00',
'area': [
30, -100, 0,
-40,
],
'date': '2021-11-15/2021-11-15',
'model_level': [
'135', '136', '137',
],
},
'download.netcdf.zip')
#------------------------------------------------------------------>
#Leemos el netcdf que acabamos de bajar
nc = Dataset('levtype_ml.nc') #ml = model level
nc1 = Dataset('levtype_pl.nc') #pl = pressure level
nc2 = Dataset('levtype_sfc.nc') #sfc = surface
#Inventario del contenido del archivo 1
for key,value in nc.variables.items():
print(key)
print(value)
print()
#Inventario del contenido del archivo 1
for key,value in nc1.variables.items():
print(key)
print(value)
print()
#Inventario del contenido del archivo 1
for key,value in nc2.variables.items():
print(key)
print(value)
print()
#Lectura de latitudes y longitudes
lat = nc.variables['latitude'][:]
zlon = nc.variables['longitude'][:]
#Lectura de niveles (Si los hubiere)
#nivel = nc.variables['level'][:]
#Construyendo el vector de tiempos
#Lectura y formato del tiempo
unidades = nc.variables['time'].units
calendario = nc.variables['time'].calendar
tiempo = nc.variables['time'][:]
tiempo = num2date(tiempo, units=unidades,calendar=calendario)
tiempo = [i.strftime("%d-%m-%Y %H:%M") for i in tiempo]
#Lectura de variables, en este caso, contaminantes (Se omite el nivel porque no hay)
ozono = nc.variables['go3'][:,:,:,:]
#Seleccionamos el tiempo, para este caso no tenemos nivel
#ilevel = 0
itime=len(tiempo)-1 #Asumo que el vector comienza en 0
variable = ozono[itime,1,:,:]
#Ahora realizamos un plot simple seleccionando un tiempo
fig=plt.figure(figsize=(11,5))
ax=fig.add_subplot(1,1,1,projection=ccrs.Mercator())
#Esteticos
ax.add_feature(cfeature.COASTLINE,lw=.5)
ax.add_feature(cfeature.RIVERS,lw=0.5)
ax.add_feature(cfeature.BORDERS, lw=0.6)
#Cabezera
ax.set_title('TCNO2 %s' % tiempo[itime])
#Lineas
ax.gridlines(xlocs=np.arange(-180,180,2.5),
            ylocs=np.arange(-90,90,2.5),
draw_labels=True,color='gray',lw=0.1)
pc=ax.contourf(zlon,lat,variable,
levels=30,
transform=ccrs.PlateCarree(),
cmap='twilight',
)
cbar = fig.colorbar(pc)
plt.show()