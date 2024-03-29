# ecosite

#' Convert Ecological Site ID to Ecological Site Name using NASIS
#'
#' @param x character vector of Ecological Site IDs
#'
#' @details Uses contents of local NASIS `ecologicalsite` table
#' @export
ecositeid_to_name <- function(x) {
  stopifnot(requireNamespace("odbc"))
  lutdf <- soilDB::dbQueryNASIS(soilDB::NASIS(), paste0("SELECT DISTINCT ecositeid, ecositenm FROM ecologicalsite WHERE ecositeid IN ", soilDB::format_SQL_in_statement(unique(x))))
  lut <- lutdf$ecositenm
  names(lut) <- lutdf$ecositeid
  lut[x]
}
