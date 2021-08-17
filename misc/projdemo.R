library(sf)
#> Warning: package 'sf' was built under R version 4.0.5
#> Linking to GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
library(NASIStools)

# get first muname starts with musick
qry <- "SELECT TOP 1 mukey FROM mapunit WHERE muname LIKE 'Musick%'"
x <- sf::st_as_sf(soilDB::fetchSDA_spatial(soilDB::SDA_query(qry)$mukey,
                                           add.fields = 'mapunit.muname'))
#> single result set, returning a data.frame
#> Using 1 chunks...
#> Chunk #1 completed (n = 1; 2.2 secs)
#> Done in 4.8 secs; mean/chunk: 2.2 secs; mean/symbol: 4.85 secs.

# single delineation
object <- x[1, ]

best_proj_pipeline(object, "OGC:CRS83", "OGC:CRS27")
#> OGC:CRS83  ->   OGC:CRS27  :: selected pipeline  24  of  52 :
#>    +proj=pipeline +step +proj=unitconvert +xy_in=deg +xy_out=rad +step +proj=push +v_3 +step +proj=cart +ellps=GRS80 +step +inv +proj=helmert +x=0.9956 +y=-1.9013 +z=-0.5215 +rx=0.025915 +ry=0.009426 +rz=0.011599 +s=0.00062 +convention=coordinate_frame +step +inv +proj=helmert +x=2.478 +y=149.752 +z=197.726 +rx=-0.526 +ry=-0.498 +rz=0.501 +s=0.685 +convention=coordinate_frame +step +inv +proj=cart +ellps=clrk66 +step +proj=pop +v_3 +step +proj=unitconvert +xy_in=rad +xy_out=deg

best_proj_pipeline(object, "OGC:CRS83", "EPSG:4267")
#> OGC:CRS83  ->   EPSG:4267  :: selected pipeline  16  of  25 :
#>    +proj=pipeline +step +proj=unitconvert +xy_in=deg +xy_out=rad +step +proj=push +v_3 +step +proj=cart +ellps=WGS84 +step +inv +proj=helmert +x=2.478 +y=149.752 +z=197.726 +rx=-0.526 +ry=-0.498 +rz=0.501 +s=0.685 +convention=coordinate_frame +step +inv +proj=cart +ellps=clrk66 +step +proj=pop +v_3 +step +proj=unitconvert +xy_in=rad +xy_out=deg

best_proj_pipeline(object, "EPSG:4326", "EPSG:4267")
#> EPSG:4326  ->   EPSG:4267  :: selected pipeline  14  of  31 :
#>    +proj=pipeline +step +proj=unitconvert +xy_in=deg +xy_out=rad +step +proj=push +v_3 +step +proj=cart +ellps=WGS84 +step +inv +proj=helmert +x=2.478 +y=149.752 +z=197.726 +rx=-0.526 +ry=-0.498 +rz=0.501 +s=0.685 +convention=coordinate_frame +step +inv +proj=cart +ellps=clrk66 +step +proj=pop +v_3 +step +proj=unitconvert +xy_in=rad +xy_out=deg

best_proj_pipeline(object, "EPSG:6350", "EPSG:4267")
#> EPSG:6350  ->   EPSG:4267  :: selected pipeline  1  of  1 :
#>    +proj=pipeline +step +inv +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +step +proj=unitconvert +xy_in=rad +xy_out=deg 
#> 
#> Tried  1  pipelines

best_proj_pipeline(object, "EPSG:6350", "OGC:CRS27")
#> EPSG:6350  ->   OGC:CRS27  :: selected pipeline  21  of  33 :
#>   +proj=pipeline +step +inv +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +step +proj=push +v_3 +step +proj=cart +ellps=GRS80 +step +inv +proj=helmert +x=0.99343 +y=-1.90331 +z=-0.52655 +rx=0.02591467 +ry=0.00942645 +rz=0.01159935 +s=0.00171504 +convention=coordinate_frame +step +inv +proj=helmert +x=2.478 +y=149.752 +z=197.726 +rx=-0.526 +ry=-0.498 +rz=0.501 +s=0.685 +convention=coordinate_frame +step +inv +proj=cart +ellps=clrk66 +step +proj=pop +v_3 +step +proj=unitconvert +xy_in=rad +xy_out=deg 
