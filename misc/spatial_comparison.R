# rasterise one or more GDBs
library(sf)

gdb.path <- "E:/CA649/Geodata/GDB"
gdb.name <- file.path(gdb.path, c(
  "FGDB_CA750_Join_Project_2021_0722_agb.gdb",
  "FGDB_CA731_Join_Project_2021_0722_agb.gdb",
  "FGCA649_Projects_2021_0722_agb.gdb/"
))
gdb.sym <-gsub(".*([A-Z]{2}[0-9]{3})_.*", "\\1", gdb.name)

dsn <- "E:/workspace/Cochran_InterpCompare/CA649_export_after/"
NASIStools::createSSURGO(dsn, output_path = paste0(dsn,"/","big.sqlite"))
dbp <- paste0(dsn,"/","big.sqlite")
keys <- NASIStools::get_SSURGO_component_keys(dbp)

geoms <- lapply(seq_along(gdb.name), function(i)  {
  sf::st_transform(sf::read_sf(gdb.name[i], layer=paste0(tolower(gdb.sym[i]),"_a")), sf::st_crs(32610))
})

geom <- do.call('rbind', lapply(geoms, function(x) x[,"MUSYM"]))
geom <- merge(geom, keys, by.x = "MUSYM", by.y = "musym")
geom$lmapunitiid <- as.integer(geom$lmapunitiid)
raster_template <- raster::raster(geom, res=100)
ras <- fasterize::fasterize(geom, raster_template, field = 'lmapunitiid')

ssas <- sf::st_as_sf(soilDB::fetchSDA_spatial(c("CA630","CA649",
                                                "CA731","CA750"),
                                              "areasymbol", geom.src="sapolygon"))
ssas <- sf::st_transform(ssas, sf::st_crs(geom))

raster::plot(ras)
plot(sf::st_cast(sf::st_geometry(ssas), 'MULTILINESTRING'), add = TRUE)
