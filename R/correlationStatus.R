# NASIS mapunit correlation helpers

#' Get Correlated Mapunit and Component Information
#'
#' Get the correlated mapunit status for a vector of IDs. These IDs could be from area, legend, legendmapunit, mapunit correlation, data mapunit, component, component ecological site or ecological site.
#'
#' @param x vector of record IDs (Default `NULL` is entire local database)
#' @param x.col physical column name containing record IDs (e.g. liid, muiid, dmuiid, coiid, ecositeiid). Default `"dmuiid"`
#' @param additional include additional mapunits? Default `FALSE`
#' @return a data.frame containing component-level information and IDs for all parent tables up to legend. Results will contain component information from all related mapunits.
#' @export
get_correlation_info <- function(x = NULL, x.col = "dmuiid", additional = FALSE) {

  # allow a variety of inputs to find target mapunits
  x1 <- soilDB::uncode(soilDB::dbQueryNASIS(soilDB::NASIS(),
                        sprintf("SELECT DISTINCT correlation.dmuiidref FROM area
                                 INNER JOIN legend ON area.areaiid = legend.areaiidref
                                 INNER JOIN lmapunit ON legend.liid = lmapunit.liidref
                                 INNER JOIN mapunit ON lmapunit.muiidref = mapunit.muiid
                                 INNER JOIN correlation ON mapunit.muiid = correlation.muiidref
                                 INNER JOIN datamapunit ON correlation.dmuiidref = datamapunit.dmuiid
                                 INNER JOIN component ON datamapunit.dmuiid = component.dmuiidref
                                 INNER JOIN coecosite ON coecosite.coiidref = component.coiid
                                 INNER JOIN ecologicalsite ON ecologicalsite.ecositeiid = coecosite.ecositeiidref
                                 %s",
                                ifelse(length(x) > 0,
                                       sprintf("WHERE %s IN %s", x.col, soilDB::format_SQL_in_statement(unique(x))),
                                       ""))))

  # get correlated muiids based on input dmuiid
  x2 <- soilDB::uncode(soilDB::dbQueryNASIS(soilDB::NASIS(),
                        sprintf("SELECT lmapunit.muiidref FROM legend
                                 INNER JOIN lmapunit ON legend.liid = lmapunit.liidref
                                 INNER JOIN mapunit ON lmapunit.muiidref = mapunit.muiid
                                 INNER JOIN correlation ON mapunit.muiid = correlation.muiidref
                                 WHERE correlation.dmuiidref IN %s",
                                soilDB::format_SQL_in_statement(unique(x1$dmuiidref)))))

  # get component-level information for representative dmus for each muiid
  res <- soilDB::uncode(soilDB::dbQueryNASIS(soilDB::NASIS(),
                        sprintf("SELECT liidref, lmapunitiid, correlation.dmuiidref, lmapunit.muiidref,
                                        muname, mustatus, nationalmusym, dmudesc, areasymbol, musym,
                                        coiid AS coiidref, compname, compkind, comppct_r, majcompflag,
                                        ecositeiidref, ecositeid, ecositenm
                                 FROM area
                                 INNER JOIN legend ON area.areaiid = legend.areaiidref
                                 INNER JOIN lmapunit ON legend.liid = lmapunit.liidref
                                 INNER JOIN mapunit ON lmapunit.muiidref = mapunit.muiid
                                 INNER JOIN correlation ON mapunit.muiid = correlation.muiidref
                                 INNER JOIN datamapunit ON correlation.dmuiidref = datamapunit.dmuiid AND repdmu = 1
                                 INNER JOIN component ON datamapunit.dmuiid = component.dmuiidref
                                 INNER JOIN coecosite ON coecosite.coiidref = component.coiid
                                 INNER JOIN ecologicalsite ON ecologicalsite.ecositeiid = coecosite.ecositeiidref
                                 WHERE correlation.muiidref IN %s",
                                soilDB::format_SQL_in_statement(unique(x2$muiidref)))))

  if (!additional) {
    return(subset(res, res$mustatus != "additional"))
  }
  res
}
