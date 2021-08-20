library(sf)

# spatial comparisons
basepath <- "E:/CA649/QA/ReconcileSpatial"
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
gdb_a <- lapply(seq_len(nrow(gdbs)), function(i) sf::st_read(gdbs$path[i], gdbs$layer[i]))
gdb_b <- lapply(seq_len(nrow(gdbs)), function(i) sf::st_read(gdbs$path[i], gdbs$boundary[i]))
gdb_musym <- lapply(gdb_a, function(x) {
  unique(paste0(x$AREASYMBOL, x$MUSYM))
})

# alpha sort to assign codes
newcodes <- sort(unique(do.call('c', gdb_musym)))
mucode_lut <- 1:length(newcodes)
names(mucode_lut) <- newcodes
mucode_inv <- names(mucode_lut)
names(mucode_inv) <- mucode_lut

rasts <- lapply(seq_along(gdb_a), function(i) {
  x <- gdb_a[[i]]

  # assign a numeric value corresponding to MUSYM on particular legend
  x$mucode <- mucode_lut[paste0(x$AREASYMBOL, x$MUSYM)]

  fasterize::fasterize(x, field = 'mucode', fasterize::raster(x, res = 30))
})

gdbs$layer <- gdb_a
gdbs$boundary <- gdb_b
gdbs$rasts <- rasts

raster::plot(rasts[[1]])
raster::plot(rasts[[2]])
raster::plot(rasts[[3]])

# ca649
# amask <- raster::mask(rasts[[2]], rasts[[2]], inverse = TRUE,
#                       maskvalue = mucode_lut["CA649JeF2"])
# spnewcodes <- raster::values(raster::mask(rasts[[3]], amask))

#CA731
# amask <- raster::mask(rasts[[4]], rasts[[4]], inverse = TRUE,
#                       maskvalue = mucode_lut["CA731BeFma"])
# spnewcodes <- raster::values(raster::mask(rasts[[5]], amask))

# ca750
# amask <- raster::mask(rasts[[6]], rasts[[6]], inverse = TRUE,
#                       maskvalue = mucode_lut["CA750JcFma"])
# spnewcodes <- raster::values(raster::mask(rasts[[7]], amask))
# sort(round(prop.table(table(mucode_inv[spnewcodes])) * 100), decreasing=TRUE)

# get boundaries

byarea <- split(gdbs, f = gdbs$areasymbol)
area_change <- vector('list', length(byarea))
names(area_change) <- unique(gdbs$areasymbol)
new_areas <- area_change
old_areas <- area_change

for (i in names(area_change)) {
  area <- byarea[[i]]

  # get the newest and oldest raster
  new_areas[[i]] <- area$rasts[[which.max(area$FY)]]
  old_areas[[i]] <- area$rasts[[which.min(area$FY)]]

  # identify areas that changed MUSYM over course of project
  area_change[[i]] <- old_areas[[i]] - new_areas[[i]]

  # if they did not change, value is zero, map them to NA to highlight areas that changed
  unchanged.idx <- which(raster::values(area_change[[i]] == 0))
  raster::values(area_change[[i]])[unchanged.idx] <- NA

  # remove the values where they did not change
  raster::values(new_areas[[i]])[unchanged.idx] <- NA
  raster::values(old_areas[[i]])[unchanged.idx] <- NA
}

# inspect
for (i in names(area_change)) {
  bdy <- sf::st_cast(sf::st_geometry(byarea[[i]]$boundary[[1]]), 'MULTILINESTRING')
  par(mfrow=c(1,2))
  raster::plot(old_areas[[i]], main = paste(i, "BEFORE"))
  plot(bdy, add = TRUE)
  raster::plot(new_areas[[i]], main = paste(i, "AFTER"))
  plot(bdy, add = TRUE)

  # par(mfrow=c(1,1))
  # raster::plot(area_change[[i]], main = i)
  # # use original boundary
  # # TODO: add toggle to use "final" boundary
  # plot(sf::st_cast(sf::st_geometry(byarea[[i]]$boundary[[1]]), 'MULTILINESTRING'), add = TRUE)
}

# now, summarize each additional musym to determine composition with respect to new
# this is an index of the changes in correlation; ideally there should be locally better data/detail conveyed for these older broadly defined mapunits

# then, summarize each new musym to determine what they replaced
# this is used to choose the most appropriate component(s) to compare a new component to

