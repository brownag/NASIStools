#' Identify possible transformation pipelines
#'
#' @param object an sf object
#' @param source_crs source CRS
#' @param target_crs target CRS
#' @param require_inverse require that all transformations are invertible? Default: `TRUE`
#' @param allow_ballpark allow for "ballpark" transformations? Passed to PROJ
#' @param verbose print messages to standard output? Default `TRUE`
#'
#' @return invisible character string containing proj pipeline; if `verbose` an explanation is printed to console
#' @export
#'
#' @examples
#'
#' if (requireNamespace(sf)){
#'
#'   #  sf::sf_proj_search_paths(c(sf::sf_proj_search_paths(), "C:/Users/Andrew.G.Brown/AppData/Local/proj/"))
#'
#'   # get first muname starts with musick
#'   qry <- "SELECT TOP 1 mukey FROM mapunit WHERE muname LIKE 'Musick%'"
#'   x <- sf::st_as_sf(soilDB::fetchSDA_spatial(soilDB::SDA_query(qry)$mukey,
#'                                              add.fields = 'mapunit.muname'))
#'
#'   # single delineation
#'   object <- x[1,]
#'
#'   best_proj_pipeline(object, "OGC:CRS83", "OGC:CRS27")
#'
#'   best_proj_pipeline(object, "OGC:CRS83", "EPSG:4267")
#'
#'   best_proj_pipeline(object, "EPSG:4326", "EPSG:4267")
#'
#'   best_proj_pipeline(object, "EPSG:6350", "EPSG:4267")
#'
#'   best_proj_pipeline(object, "EPSG:6350", "OGC:CRS27")
#' }
#### @importFrom sf sf_proj_pipelines st_transform st_as_sf st_bbox st_crs
best_proj_pipeline <-  function(object,
                                source_crs,
                                target_crs,
                                require_inverse = FALSE,
                                allow_ballpark = TRUE,
                                verbose = TRUE) {
  if (requireNamespace("sf")) {
    pipes <- sf::sf_proj_pipelines(source_crs = source_crs, target_crs = target_crs)
    pipes <- pipes[pipes$definition != "+",]

    # pick first, best instanstiable operation
    idx <- which.min(pipes$accuracy)[1]
    pipe.sub <- pipes[idx, ]

    # use the transformation
    znew <- suppressWarnings(
      sf::st_transform(
        object,
        target_crs,
        pipeline = pipe.sub$definition,
        allow_ballpark = allow_ballpark
      )
    )

    .selectpipeline <- function(i) cat(source_crs, ' ->  ', target_crs, ' :: selected pipeline ',
                                       i, ' of ', nrow(pipes), ':\n\t ', pipes$definition[i], "\n\n")

    if (is.na(as.numeric(sf::st_bbox(znew))[1]))  {

      # sort on accuracy and remove pipelines without an inverse
      pipes <- pipes[order(pipes$accuracy, decreasing = TRUE), ]
      if (require_inverse) {
        pipes <- pipes[pipes$has_inverse, ]
      }

      # check all subsequent pipelines until one works
      for (i in 2:nrow(pipes)) {
        znew <- try(suppressWarnings(sf::st_transform(object, target_crs,
                                                      pipeline = pipes$definition[i],
                                                      allow_ballpark = allow_ballpark)))
        if (inherits(znew, 'try-error')) {
          next
        }
        if (!is.na(as.numeric(sf::st_bbox(znew))[1])) {
          if (verbose) .selectpipeline(i)
          def <- pipes$definition[i]
          if (verbose) cat("Tried ", i, " pipelines")
          break
        }
      }
    } else {
      if (verbose) .selectpipeline(idx)
      def <- pipe.sub$definition[1]
    }
    invisible(def)
  }
}
