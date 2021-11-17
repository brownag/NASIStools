
#' get_projectmapunits_by_uprojectid
#'
#' @param uprojectid vector of user project IDs
#' @param areasymbol Soil Survey Legend areasymbol to include in WHERE clause; Default: `NULL` is no constraint on areasymbol
#' @param dsn data source name, default is `soilDB::NASIS()` _OdbcConnection_
#'
#' @return data.frame result
#' @export
#' @importFrom soilDB dbQueryNASIS NASIS format_SQL_in_statement
get_projectmapunits_by_uprojectid <- function(uprojectid, areasymbol = NULL, dsn = soilDB::NASIS()) {
  q <- sprintf("SELECT * FROM mapunit
                INNER JOIN lmapunit ON lmapunit.muiidref = mapunit.muiid
                INNER JOIN legend ON legend.liid = lmapunit.liidref
                INNER JOIN area ON area.areaiid = legend.areaiidref
                INNER JOIN projectmapunit ON projectmapunit.muiidref = mapunit.muiid
                INNER JOIN project ON project.projectiid = projectmapunit.projectiidref
                WHERE uprojectid IN %s%s",
               soilDB::format_SQL_in_statement(uprojectid),
               ifelse(is.null(areasymbol), "", sprintf(" AND areasymbol IN %s",
                                                       soilDB::format_SQL_in_statement(areasymbol))))
  soilDB::dbQueryNASIS(dsn,  q)
}

#' get_lmapunit_by_areasymbol
#'
#' @param areasymbol vector of areasymbols
#' @param dsn data source name, default is `soilDB::NASIS()` _OdbcConnection_
#'
#' @return data.frame result
#' @export
#' @importFrom soilDB dbQueryNASIS NASIS format_SQL_in_statement
get_lmapunit_by_areasymbol <- function(areasymbol, dsn = soilDB::NASIS()) {
  q <- sprintf("SELECT * FROM mapunit
                INNER JOIN lmapunit ON lmapunit.muiidref = mapunit.muiid
                INNER JOIN legend ON legend.liid = lmapunit.liidref
                INNER JOIN area ON area.areaiid = legend.areaiidref
                WHERE areasymbol IN %s", soilDB::format_SQL_in_statement(areasymbol))
  soilDB::dbQueryNASIS(dsn,  q)
}
