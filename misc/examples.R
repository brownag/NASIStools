# # dsn: local SSURGO export (containing cointerp table for new mapunits)

library(NASIStools)
library(data.table)

mrulename <-  "WMS - Pond Reservoir Area" #"FOR - Mechanical Site Preparation (Surface)"

prjdir <- "E:/workspace/Cochran_InterpCompare"
tabdir_before <- "E:/workspace/Cochran_InterpCompare/ca649_export_test"
tabdir_after <- "E:/workspace/Cochran_InterpCompare/CA649_export_after"

before <- file.path(tabdir_before, "CA649_export_before.sqlite")
after <- file.path(tabdir_after, "CA649_export_after.sqlite")

createSSURGO(tabdir_before, output_path = before)
createSSURGO(tabdir_after, output_path = after)

dsn_before <- DBI::dbConnect(RSQLite::SQLite(), before)
dsn <- dsn_before
dsn_after <- DBI::dbConnect(RSQLite::SQLite(), after)

result_before <- data.table(get_SSURGO_interp_reasons_by_mrulename(dsn_before, mrulename))
result_after <- data.table(get_SSURGO_interp_reasons_by_mrulename(dsn_before, mrulename))

result_after$lmapunitiid %in% result_before$lmapunitiid
