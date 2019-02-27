
###
### Some helpful .r functions and examples for working with shapefiles
###
### 2018-12-03 SAW
### 

## Import a shapefile
library(rgdal)
egshp <- readOGR("./path/to/shapefile/", "shapefile_name")

## Shapefile's attribute table is in
egshp@data

## Get centroids of polygons in shapefile
egshp.cen <- gCentroid(egshp, byid=TRUE)

## Dissolve a polygon shapefile
egshp.dis <- gUnaryUnion(egshp)

## Clip points (or other shapes) to ones within a polygon
egshp.clip <- gIntersection(egshp.cen, egshp.dis)

## Plot your map(s)
plot(egshp)
plot(egshp.cen, add=T, col="red")
plot(egshp.clip, add=T, col="blue")


