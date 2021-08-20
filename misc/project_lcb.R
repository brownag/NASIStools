# create project impact extent
# basepath <- "E:/CA649/QA/ReconcileSpatial"
# uprojectid <- "2021-2SON-MLRA-006"
basepath <- "E:/CA649/QA/ReconcileSpatial"
uprojectid <- "2022-2SON-MLRA-6002"
dirs <- list.dirs(basepath, recursive = TRUE, full.names = TRUE)

dirs <- dirs[grepl("gdb", dirs)]
dirs2 <- gsub(basepath, "", dirs)
gdbs <- data.frame(do.call('rbind', lapply(strsplit(dirs2, "/"),
                                           function(x)
                                             data.frame(
                                               areasymbol = x[2],
                                               FY = x[3],
                                               gdb = x[4],
                                               layer = paste0(tolower(x[2]), "_a"),
                                               boundary = paste0(tolower(x[2]), "_b")
                                             ))), path = dirs)

gdb_new <- do.call('rbind', lapply(split(gdbs, list(gdbs$areasymbol)),
                                   function(x) x[which.max(x$FY),]))
gdb_a <- lapply(seq_len(length(gdb_new$areasymbol)), function(i)
  sf::st_read(gdb_new$path[i], gdb_new$layer[i]))

gdb_a[[1]] <- sf::st_transform(gdb_a[[1]], crs = sf::st_crs(gdb_a[[3]]))
gdb_a[[2]] <- sf::st_transform(gdb_a[[2]], crs = sf::st_crs(gdb_a[[3]]))

gdbs2 <- do.call('rbind', lapply(gdb_a, function(x) x[,c("AREASYMBOL","MUSYM")]))

x <- soilDB::dbQueryNASIS(soilDB::NASIS(), sprintf("SELECT * FROM projectmapunit_View_1
                                            INNER JOIN project_View_1 ON project_View_1.projectiid = projectmapunit_View_1.projectiidref
                                            INNER JOIN mapunit ON mapunit.muiid = projectmapunit_View_1.muiidref
                                            INNER JOIN lmapunit ON lmapunit.muiidref = mapunit.muiid
                                            INNER JOIN legend ON legend.liid = lmapunit.liidref
                                            WHERE uprojectid = '%s'", uprojectid))
plot(sf::st_transform(sf::st_as_sf(soilDB::fetchSDA_spatial(c("CA649"),
                                                            by.col = "areasymbol",
                                                            geom.src = "sapolygon")),
                      sf::st_crs(gdbs2))$geometry)
project_extent <- subset(gdbs2, gdbs2$MUSYM %in% x$musym)
plot(sf::st_geometry(project_extent), add=T)

sf::st_write(project_extent, paste0(uprojectid, ".shp"), append=FALSE)
