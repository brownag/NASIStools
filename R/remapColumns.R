#' Map columns of a SoilProfileCollection or data.frame to a custom schema
#' @param x a SoilProfileCollection or horizon-level data.frame
#' @param .map a named, nested list where each sub-list defines the columns associated with a new table in terms of existing columns in `x`. The basic format of the .map argument follows: `list(new_table_name = list(new_column_name = 'old_column_name'))`
#'
#' @return a named list of data.frame objects (equal in length to `.map`) containing data from `x` "remapped" to the schema specified by `.map`.
#' @export

# @examples
# # load example data and create a SoilProfileCollection
# library(aqp)
# data(sp1, package = "aqp")
# depths(sp1) <- id ~ top + bottom
# 
# # add some fake "coordinates" to the site slot
# sp1$x <- runif(length(sp1))
# sp1$y <- runif(length(sp1))
# 
# # format for .map argument:
# #  list(new_table_name = list(new_column_name = 'old_column_name'))
# 
# my_map <- list(site = list(siteiid = 'id', longstddecimaldegrees = 'x',latstddecimaldegrees = 'y'),
#                phorizon = list(phiid = 'hzID', hzname = 'name',
#                                hzdept = 'top', hzdepb = 'bottom',
#                                m_hue = 'hue', d_hue = 'hue',
#                                m_value = 'value', d_value = 'value',
#                                m_chroma = 'chroma', d_chroma = 'chroma'))
# 
# sp1_tables <- remapColumns(sp1, my_map)
remapColumns <- function(x, .map) {
  oldcol <- lapply(.map, as.character)
  newcol <- lapply(.map, names)
  res <- lapply(seq_along(oldcol), function(i) {
    res <- data.frame(lapply(oldcol[[i]], function(z) x[[z]]))
    colnames(res) <- newcol[[i]]
    res
  })
  names(res) <- names(.map)
  res
}

#' "Unflattening" selected columns to a table with more rows
#' @param x a data.frame
#' @param .map has format `list(foreign_key = list(primary_key = vector_of_column_defining_unique_record))`
#' @param .keys has format`list(new_column_name = vector_of_old_column_names)`
#' @export
# # create a phcolor-like table from phorizon level columns
# .map <- list(phcolor = list(phiidref = c(dry = 'phiid', moist = 'phiid'),
#                             colorhue = c(dry = "d_hue", moist = "m_hue"),
#                             colorvalue = c(dry = "d_value", moist = "m_value"),
#                             colorchroma = c(dry = "d_chroma", moist = "m_chroma")))
# .keys <- list(phiidref = list(phcoloriid = 'colormoistst'))
# head(renormalizeColumns(sp1_tables[['phorizon']], .map, .keys))
#
renormalizeColumns <- function(x, .map, .keys) {

  # TODO: iterate over .map/.keys for multiple table results from a single x
  # for now can only do one table at a time
  stopifnot(length(.keys) == 1 && length(.map) == 1)

  fkey <- names(.keys)[1]
  pkey <-  names(.keys[[1]])
  ucols <- as.character(.keys[[1]])

  newcols <- lapply(.map[[1]], function(y) names(y))
  oldcols <- lapply(.map[[1]], function(y) as.character(y))

  newdata <- as.data.frame(lapply(oldcols, function(y) do.call('c', lapply(y, function(z) x[[z]]))))

  newids <- as.data.frame(lapply(1, function(i) {
    do.call('c', lapply(seq_along(oldcols[[i]]), function(j) {
        rep(newcols[[i]][j], length(x[[oldcols[[i]][j]]]))
      }))
  }))
  colnames(newids) <- ucols

  res <- cbind(newdata, newids)
  res[[pkey]] <- 1:nrow(res)
  if (!is.numeric(res[[fkey]])){
    res[[fkey]] <- as.numeric(res[[fkey]])
  }
  res <- res[order(res[[fkey]]), ]
  rownames(res) <- NULL
  res
}

## TESTING
# my_map <- list(site = list(siteiid = 'id', longstddecimaldegrees = 'x', latstddecimaldegrees = 'y'),
#                siteobs = list(obsdate = 'date', obsdatekind = 'obsdatekind', datacollector = 'describer'),
#                pedon = list(peiid = 'id', upedonid = 'pedon_id', pedrecorigin = 'pedon_origin', descname = 'describer'),
#                phorizon = list(phiid = 'hzID', obsmethod = 'obsmethod', hzname = 'name', hzdept = 'top', hzdepb = 'bottom')
