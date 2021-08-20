
library(soilDB)

areasymbol <- "CA649"
uprojectid <- "2021-2SON-MLRA-006"

q <- sprintf("SELECT * FROM mapunit
INNER JOIN lmapunit ON lmapunit.muiidref = mapunit.muiid
INNER JOIN legend ON legend.liid = lmapunit.liidref
INNER JOIN area ON area.areaiid = legend.areaiidref
WHERE areasymbol = '%s'", areasymbol)
x1 <- dbQueryNASIS(NASIS(),  q)

q <- sprintf("SELECT * FROM mapunit
INNER JOIN projectmapunit ON projectmapunit.muiidref = mapunit.muiid
INNER JOIN project ON project.projectiid = projectmapunit.projectiidref
WHERE uprojectid = '%s'", uprojectid)
x2 <- dbQueryNASIS(NASIS(),  q)

missingx2 <- x2[!x2$nationalmusym %in% x1$nationalmusym,]$muiid

# missingx1 <- x1[!x1$nationalmusym %in% x2$nationalmusym,]
# tibble::tibble(missingx1[,c(1,5)])
# subset(missingx1, mustatus == 3)
# unique(x2$projectiid)
