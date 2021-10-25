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

res649 <- lapply(mucode_lut[grepl("CA649", names(mucode_lut))], function(x) {
  amask <- raster::mask(rasts[[3]], rasts[[3]], inverse = TRUE, maskvalue = x)
  spnewcodes <- raster::values(raster::mask(rasts[[1]], amask))
  sort(round(prop.table(table(mucode_inv[spnewcodes])) * 100), decreasing=TRUE)
})
names(res649) <- names(mucode_lut)[grepl("CA649", names(mucode_lut))]

res649inv <- lapply(mucode_lut[grepl("CA649", names(mucode_lut))], function(x) {
  amask <- raster::mask(rasts[[1]], rasts[[1]], inverse = TRUE, maskvalue = x)
  spnewcodes <- raster::values(raster::mask(rasts[[3]], amask))
  sort(round(prop.table(table(mucode_inv[spnewcodes])) * 100), decreasing=TRUE)
})
names(res649inv) <- names(mucode_lut)[grepl("CA649", names(mucode_lut))]

# all mapunits that contributed to 7103
mucode <- "CA6497103"
ret <- res649[[mucode]][res649[[mucode]] > 0]
data.frame(mucode = ret)

after <- do.call('rbind', lapply(names(mucode_lut[grepl("CA649", names(mucode_lut))]), function(x) {
  ret <- res649[[x]][res649[[x]] >= 0]
  if (length(ret) == 0)
    return(NULL)
  data.frame(after = x, before = names(ret), percentage_of_after = as.numeric(ret))
}))

before <- do.call('rbind', lapply(names(mucode_lut[grepl("CA649", names(mucode_lut))]), function(x) {
  ret <- res649inv[[x]][res649inv[[x]] >= 0]
  if (length(ret) == 0)
    return(NULL)
  data.frame(before = x, after = names(ret), percentage_of_before = as.numeric(ret))
}))

write.csv(after, "after.csv")
write.csv(before, "before.csv")

# all mapunits that Brf2 contributed to
mucode <- "CA649BrF2"
ret <- res649inv[[mucode]][res649inv[[mucode]] >= 1]
data.frame(mucode = ret)

# ca649
# amask <- raster::mask(rasts[[2]], rasts[[2]], inverse = TRUE,
#                       maskvalue = mucode_lut["CA649JeF2"])
# spnewcodes <- raster::values(raster::mask(rasts[[3]], amask))
#
amask <- raster::mask(rasts[[3]], rasts[[3]], inverse = TRUE,
                      maskvalue = mucode_lut["CA6497091"])
spnewcodes <- raster::values(raster::mask(rasts[[1]], amask))

#CA731
# amask <- raster::mask(rasts[[4]], rasts[[4]], inverse = TRUE,
#                       maskvalue = mucode_lut["CA731BeFma"])
# spnewcodes <- raster::values(raster::mask(rasts[[5]], amask))
# amask <- raster::mask(rasts[[5]], rasts[[5]], inverse = TRUE,
#                       maskvalue = mucode_lut["CA7317103"])
# spnewcodes <- raster::values(raster::mask(rasts[[4]], amask))

#
# ca750
# amask <- raster::mask(rasts[[6]], rasts[[6]], inverse = TRUE,
#                       maskvalue = mucode_lut["CA750JcFma"])
# spnewcodes <- raster::values(raster::mask(rasts[[7]], amask))
# amask <- raster::mask(rasts[[7]], rasts[[7]], inverse = TRUE,
#                       maskvalue = mucode_lut["CA7508162"])
# spnewcodes <- raster::values(raster::mask(rasts[[6]], amask))
sort(round(prop.table(table(mucode_inv[spnewcodes])) * 100), decreasing=TRUE)
muprop <- sort(prop.table(table(mucode_inv[spnewcodes])), decreasing=TRUE)
muprop
foo <- sapply(gsub("[A-Z]{2}[0-9]{3}(.*)", "\\1", names(muprop)), function(x) {cat(x, "\n"); x})
dput(as.character(foo))
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

