### SETUP
## In NASIS, go to Exports Explorer menu and Add New Export...
##
## Tab #1: Criteria
## - Select Export Target: SSURGO
## - Choose desired map units, data map units, and components
##
## Tabs #2 and #3: Interpretations and Notes
## - Select Interpretations to include in export
## - Select Text Notes to include in export
##
## Tab #4: Run Export
##  - Enter file name for ZIP
##  - Export run on server and result emailed to user
##

#' Get Interpretation Rating "Reasons" from SSURGO `cointerp` table
#'
#' @param dsn A DBIConnection
#' @param drv A DBI driver (Default: `RSQLite::SQLite()`)
#' @param mrulename Rule name of interpretation
#' @param n Number of reasons to return
#' @param close close connections that were opened internally when done? Default: `TRUE`
#'
#' @return A `data.frame` containing columns: "lmapunitiid", "coiid", "mrulename", "cokeyref", "Reasons", "liid", "muiid", "corriid", "dmuiid", "areasymbol", "musym", "compname", "comppct_r", "interphr", "interphrc","mukey"
#' @export
#' @seealso [get_SSURGO_cointerp()] [get_SSURGO_component_keys()]
#' @importFrom DBI dbConnect dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom soilDB format_SQL_in_statement
get_SSURGO_interp_reasons_by_mrulename <- function(dsn, drv = RSQLite::SQLite(),
                                                   mrulename, n = 2, close = TRUE) {
  # based on VBA Function in Report Functions module of Access .mdb
  # GetInterpReasons(strCokey As String,
  #                  strMRuleName As String,
  #                  intReasonCount As Integer) As Variant
  # channel <- DBI::dbConnect(odbc::odbc(),
  #     .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dsn))

  if (!inherits(dsn, 'DBIConnection')) {
    channel <- DBI::dbConnect(drv, dsn)
  } else {
    channel <- dsn
    close <- FALSE
    attr(channel, 'isUserDefined') <- TRUE
  }


  channel <- dsn

  # 1:1 with cointerp ruledepth = 0
  cointerpbase <- get_SSURGO_cointerp(channel, mrulename = mrulename, ruledepth = 0, close = FALSE)

  # 1:1 with cointerp ruledepth = 1
  cointerplvl1 <- get_SSURGO_cointerp(channel, mrulename = mrulename, ruledepth = 1, close = FALSE)

  # TODO: allow extend "reasons" to rules with ruledepth > 1?

  # get component data up to legend
  res2 <- DBI::dbGetQuery(channel, "SELECT legend.lkey, mapunit.mukey,
                                           mapunit.mukey AS lmapunitiid,
                                           component.cokey,
                                           mustatus, musym, muname,
                                           compname, comppct_r
                                    FROM legend
         INNER JOIN mapunit ON mapunit.lkey = legend.lkey
         INNER JOIN component ON component.mukey = mapunit.mukey")

  # unique "cokey" is mukey (SSURGO) / lmapunitiid (NASIS) ":" coiid (NASIS)

  # calculate NASIS coiid
  cointerpbase$lmapunitiid <- as.integer(gsub("(\\d+):.*", "\\1", cointerpbase$cokey))
  cointerplvl1$lmapunitiid <- as.integer(gsub("(\\d+):.*", "\\1", cointerplvl1$cokey))

  # calculate NASIS coiid
  cointerplvl1$coiid <- as.integer(gsub(".*:(\\d+)", "\\1", cointerplvl1$cokey))
  res2$coiid <- as.integer(gsub(".*:(\\d+)", "\\1", res2$cokey))
  cointerpbase$coiid <- as.integer(gsub(".*:(\\d+)", "\\1", cointerpbase$cokey))

  # extract the "high representative" rating and class for 0th level rule
  high_rep_rating_class <- cointerpbase[,c("lmapunitiid","coiid","interphr","interphrc")]
  colnames(high_rep_rating_class) <- c("lmapunitiid","coiid","interphr","interphrc")

  .SD <- NULL

  userdefined <- attributes(channel)$isUserDefined

  if ((is.null(userdefined) || !userdefined) && close) {
    DBI::dbDisconnect(channel)
  }

  # flatten the reasons so they are 1:1 with component, join to lookup tables
  as.data.frame(cointerplvl1[, list(Reasons = paste0(head(.SD[["interphrc"]], n), collapse = "; ")),
                    by = c("lmapunitiid", "coiid")][res2,
                    on = c("lmapunitiid", "coiid")][high_rep_rating_class,
                    on = c("lmapunitiid", "coiid")])
}


#' Get SSURGO Component Interpretations summaries
#'
#' @param dsn A DBIConnection
#' @param drv A DBI driver (Default: `RSQLite::SQLite()`)
#' @param columns Default: "legend.lmapunitiid", "component.coiid", "cointerp.mrulekey", "cointerp.seqnum", "legend.musym"
#' @param mrulename Filter on rule name(s) of interpretation
#' @param ruledepth Filter rule depth (default `ruledepth = 0`)
#' @param close close connections that were opened internally when done? Default: `TRUE`
#'
#' @export
#' @rdname SSURGO-export-Interpretations
#' @importFrom RSQLite SQLite
get_SSURGO_cointerp <- function(dsn, drv = RSQLite::SQLite(),
                                columns = c("mapunit.mukey",
                                            "mapunit.musym",
                                            "component.cokey",
                                            "cointerp.*"),
                                mrulename = NULL, ruledepth = 0, close = TRUE) {

  if (!inherits(dsn, 'DBIConnection')) {
    channel <- DBI::dbConnect(drv, dsn)
  } else {
    channel <- dsn
    close <- FALSE
    attr(channel, 'isUserDefined') <- TRUE
  }

  channel <- dsn

  # 1:1 with cointerp ruledepth = 0
  q <- sprintf("SELECT %s FROM cointerp
                INNER JOIN component ON component.cokey = cointerp.cokey
                INNER JOIN mapunit ON component.mukey = mapunit.mukey
                INNER JOIN legend ON legend.lkey = mapunit.lkey
                %s %s %s %s
                ORDER BY interphr DESC",
               paste0(columns, collapse = ", "),
               ifelse(!is.null(mrulename) | !is.null(ruledepth), "WHERE" , ""),
               ifelse(!is.null(mrulename), paste0("mrulename IN ",
                                                  soilDB::format_SQL_in_statement(as.character(mrulename))), ""),
               ifelse(!is.null(mrulename) & !is.null(ruledepth), "AND" , ""),
               ifelse(!is.null(ruledepth), paste0("ruledepth IN ",
                                                  soilDB::format_SQL_in_statement(as.character(ruledepth))), ""))

  res <- data.table::as.data.table(DBI::dbGetQuery(channel, q))
  userdefined <- attributes(channel)$isUserDefined

  if((is.null(userdefined) || !userdefined) && close) {
    DBI::dbDisconnect(channel)
  }

  res
}

#' @export
#' @rdname SSURGO-export-Interpretations
get_SSURGO_component_keys <- function(dsn, drv = RSQLite::SQLite(),
                                      mrulename = NULL, ruledepth = 0, close = TRUE) {

  if (!inherits(dsn, 'DBIConnection')) {
    channel <- DBI::dbConnect(drv, dsn)
  } else {
    channel <- dsn
    close <- FALSE
    attr(channel, 'isUserDefined') <- TRUE
  }

  cointerpbase <- get_SSURGO_cointerp(channel, mrulename = mrulename, ruledepth = 0)

  # identify the key components of the cointerp table to relate to NASIS
  cointerpkey <- data.frame(do.call('rbind', strsplit(cointerpbase$cointerpkey, ":")), musym = cointerpbase$musym)
  colnames(cointerpkey) <- c("lmapunitiid", "coiid", "mrulekey", "seqnum" ,"musym")

  # the unique subset of the lmapunitiid/mukey and coiid gives us a 1:1 with components
  componentkey <- unique(cointerpkey[, c("lmapunitiid", "coiid", "musym")])
  componentkey$cokey <- paste0(componentkey$lmapunitiid, ":", componentkey$coiid)
  componentkey
}

# internal function to get the full set of ids from NASIS to alias to export
# res2 <- .get_SSURGO_export_iid_table(cointerpkey$coiid)
# .get_SSURGO_export_iid_table <- function(coiids) {
#   soilDB::uncode(soilDB::dbQueryNASIS(soilDB::NASIS(), sprintf("
#     SELECT liid, lmapunitiid, muiid, corriid, dmuiid, coiid,
#            areasymbol, mustatus, musym, muname, compname, comppct_r
#     FROM area
#       INNER JOIN legend ON legend.areaiidref = area.areaiid
#       INNER JOIN lmapunit ON lmapunit.liidref = legend.liid
#       INNER JOIN mapunit ON mapunit.muiid = lmapunit.muiidref
#       INNER JOIN correlation ON correlation.muiidref = mapunit.muiid
#       INNER JOIN datamapunit ON datamapunit.dmuiid = correlation.dmuiidref
#       INNER JOIN component ON component.dmuiidref = datamapunit.dmuiid
#     WHERE component.coiid IN %s", soilDB::format_SQL_in_statement(coiids))))
# }
