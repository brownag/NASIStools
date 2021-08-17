library(data.table)
library(NASIStools)
library(magrittr)

bp <- "E:/workspace/Cochran_InterpCompare"
pth1 <- file.path(bp, "ca649_export_test")
dsn <- pth1
pth2 <- file.path(bp, "ca649_export_after")

db1 <- file.path(pth1, "before.sqlite")
db2 <- file.path(pth2, "after.sqlite")

createSSURGO(pth1, output_path = db1, overwrite = TRUE)
createSSURGO(pth2, output_path = db2, overwrite = TRUE)

mrulename <-  "WMS - Pond Reservoir Area"

con1 <-  DBI::dbConnect(RSQLite::SQLite(), db1)
x1 <- con1 %>%
  get_SSURGO_interp_reasons_by_mrulename(mrulename) %>%
  data.table(group = "BEFORE")
x1k <- get_SSURGO_component_keys(con1) %>%
  data.table(group = "BEFORE")

# cool, can use soilDB::get_SDA queries taht are simple against SQLite:
# DBI::dbGetQuery(con1, soilDB::get_SDA_coecoclass(areasymbols = "CA649", query_string = TRUE))


DBI::dbDisconnect(con1)

con2 <- DBI::dbConnect(RSQLite::SQLite(), db2)
x2 <- con2 %>%
  get_SSURGO_interp_reasons_by_mrulename(mrulename) %>%
  data.table(group = "AFTER")
x2k <- get_SSURGO_component_keys(con2) %>%
  data.table(group = "AFTER")
DBI::dbDisconnect(con2)

x <- rbind(x1, x2)

xold <- x[x$cokey %in% x1k$cokey & x$mustatus == "Additional", ]
xold
xnew <- x[!x$cokey %in% x1k$cokey & x$mustatus != "Additional", ]
xnew

boomernew <- xnew[grep("Bigridge|Minniecreek", xnew$compname),]
boomerold <- xold[grep("Boomer|Blasingame|Auburn|Trabuco", xold$compname),]

josephinenew <- xnew[grep("Nedsgulch|Wallyhill", xnew$compname),]
josephineold <- xold[grep("Josephine", xold$compname),]

compare_ratings <- function(x, y) {
  t1 <- table(x, exclude = "Not rated")
  t2 <- table(y, exclude = "Not rated")

  p1 <- prop.table(t1)
  p2 <- prop.table(t2)

  res <- data.frame(
    x.not_rated = length(x) - sum(t1),
    y.not_rated = length(y) - sum(t2),
    x.table = I(list(p1)),
    y.table = I(list(p2)),
    matches = p1 == p2
  )

  res$x.prop_not_rated <- res$x.not_rated / length(x)
  res$y.prop_not_rated <- res$y.not_rated / length(y)

  rownames(res) <- NULL
  tibble::tibble(res)
}

compare_ratings(boomerold$interphrc, boomernew$interphrc)
compare_ratings(josephineold$interphrc, josephinenew$interphrc)

prjextent <- soilDB::fetchSDA_spatial(x1k$lmapunitiid, method = "bbox")
prjextent <- sf::st_as_sf(prjextent)
prjextent2 <- sf::st_bbox(sf::st_transform(prjextent), sf::st_crs(6350))

options(timeout=60)
ssurgo.grd <- soilDB::mukey.wcs(prjextent)
raster::plot(ssurgo.grd)
