#' Read Worksheet Import Mapping XML 
#'  
#' Reads a worksheet import map, identifies physical column names and associated columns for worksheets.
#'  
#' @param wsname Worksheet Name
#'
#' @return data.frame
#' @export
#' @importFrom soilDB NASIS dbQueryNASIS 
#' @importFrom xml2 read_xml xml_children xml_attrs xml_attr
read_import_mapping <- function(wsname) {
  .SD <- NULL; .BY <- NULL; .GRP <- NULL

  x <- read_xml(dbQueryNASIS(
    NASIS(),
    sprintf("SELECT wsimportmapping FROM wsimportmap WHERE wsname = '%s'", wsname)
  )[[1]])
  
  stage1 <- xml_children(x) 
  # a <- data.table::rbindlist(lapply(xml_attrs(stage1), function(y)
  #        as.data.frame(t(y))), fill = TRUE)
  
  stage2 <- data.table::rbindlist(lapply(stage1, function(y) 
               data.frame(
                 table_name = xml_attrs(y)[1][length(xml_attr(xml_children(y), "column")) > 0],
                 column = xml_attr(xml_children(y), "column"),
                 logical_name = xml_attr(xml_children(y), "name"),
                 primary = xml_attr(xml_children(y), "primary"),
                 type = xml_attr(xml_children(y), "type"),
                 constantvalue = xml_attr(xml_children(y), "constantvalue"),
                 row.names = NULL
               )))
  
  .createStage3 <- function(y, g) {
    res <- data.frame(
      lookupname = xml_attr(xml_children(y), "name")[which(xml_attr(xml_children(y), "type") == "lookup")],
      lookupcolumn = xml_attr(xml_children(xml_children(y)), "column"),
      logical_name = xml_attr(xml_children(xml_children(y)), "name"),
      constantvalue = xml_attr(xml_children(xml_children(y)), "constantvalue"),
      row.names = NULL
    )
    res$table_name <- xml_attr(y, "name")[nrow(res) > 0]
    res$lookupid <- rep(g, nrow(res))
    res
  }
  
  stage3 <- data.table::data.table(V1 = stage1)[, .createStage3(.SD[[1]], .GRP), 
                                                  by = list(i=seq_len(length(stage1)))]
  stage3$i <- NULL
  
  .flattenLookup <- function(y) {
    idx <- which(is.na(y$lookupcolumn))
    ifelse(length(idx) > 0, paste0(get_NASIS_table_metadata(column = y$logical_name[idx], 
                                                            what.column = "ColumnLogicalName")$TableLogicalName,
                                   ".", y$logical_name[idx], ":", y$constantvalue[idx]), NA_character_)
  }
  
  flt <- stage3[, list(description = .flattenLookup(.SD)), by = c("lookupid", "lookupname")]
  stage4 <- stage3[!is.na(stage3$lookupcolumn),][flt, on = c("lookupid", "lookupname")]
  stage4$lookupid <- NULL
  stage5 <- subset(stage2, !is.na(column) & is.na(primary))
  
  res <- data.table::rbindlist(list(stage4, stage5), fill = TRUE)
  
  res$lookup_name <- NA_character_
  res$lookup_name[is.na(res$column)] <- res$logical_name[is.na(res$column)]
  res$logical_name[is.na(res$column)] <- res$lookupname[is.na(res$column)] 
  res$column[is.na(res$column)] <- res$lookupcolumn[is.na(res$column)]
  
  res$lookupcolumn <- NULL
  res$lookupname <- NULL
  res$constantvalue <- NULL
  res$primary <- NULL
  res$type <- NULL
  
  res <- res[order(nchar(res$column), res$column), ]
  m <- soilDB::get_NASIS_table_metadata()
  .finalStage <- function(y, g) {
    z <- m[which((m$TableLogicalName == g$table_name | m$TablePhysicalName == g$table_name) & 
                   m$ColumnLogicalName == g$logical_name),]
    y$table_physical_name <- z$TablePhysicalName
    y$physical_name <- z$ColumnPhysicalName
    y
  }
  res[, .finalStage(.SD, .BY), 
      by = c("table_name", "logical_name")][, .SD, 
                                            .SDcols = c("table_name", "logical_name", "column", 
                                                        "table_physical_name", "physical_name", "lookup_name",
                                                        "description")]
}
