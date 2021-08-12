#' Tools for reading NASIS exports
#'
#' Read from a tabular SSURGO export folder (.txt) files, and convert to SQLite database.
#'
#' @param dsn Path to `/tabular/` folder of a NASIS export or product available from Web Soil Survey.
#' @param output_path Path to output SQLite file or an existing `DBIConnection`
#' @param overwrite Default: `FALSE`; passed to `DBI::dbWriteTable()`
#' @param append Default: `FALSE`; passed to `DBI::dbWriteTable()`
#'
#' @return an invisible named list of data.frame. If `output_path` is not `NULL` logical; `TRUE` all tables are written to path
#' @export
#' @importFrom stats aggregate
#' @importFrom utils head read.table
createSSURGO <- function(dsn, output_path = NULL, overwrite = FALSE, append = FALSE) {

  # if we are given a path to a "tabular" folder
  if (dir.exists(dsn)){

    # list all txt files
    f1 <- list.files(dsn, "txt")

    if (length(f1) == 0) {
      stop("No TXT files found in `dsn` (", dsn, ")", call. = FALSE)
    }

    # get table list
    x1 <- read.table(file.path(dsn, "mstab.txt"), sep="|")

    # get column list
    x2 <- read.table(file.path(dsn, "mstabcol.txt"), sep="|")

    # read in tables by name from text files
    x3 <- lapply(file.path(dsn, paste0(x1$V5, ".txt")), function(x) {
      if (file.exists(x)) try(read.table(x, sep = "|"), silent = TRUE)
    })

    # find which ones are available to use
    can_access <- !sapply(x3, function(x) is.null(x) || inherits(x, 'try-error'))
    names(x3) <- x1$V5
    x3 <- x3[can_access]

    # match up the txt file table names with the physical table names
    x1.sub <- x1[x1$V5 %in% names(x3), ]
    x2.sub <- x2[x2$V1 %in% x1.sub$V1, ]
    x1x2match <- match(x2.sub$V1, x1.sub$V1)
    x2lut <- unique(x1.sub$V5[x1x2match])
    names(x2lut) <- unique(x1.sub$V1[x1x2match])

    # calculate the column names to replace V1,V2,V3... for each table
    x4 <- aggregate(x2.sub$V3,
                    by = list(grp = x2lut[x2.sub$V1]),
                    function(x) as.character(x))
    x4 <- x4[match(x1.sub$V5, x4$grp),]
    x4$physname <- names(x3)

    # replace column names
    x5 <- lapply(seq_len(nrow(x4)), function(i) {
      try({
        x <- x3[[x4$grp[i]]]
        if (!is.null(x)) {
          colnames(x) <- x4$x[i][[1]]
          x
        }
      })
    })

    # set the name of the list result to the physical table name
    names(x5) <- unique(names(x2lut[x2.sub$V1]))

    # if output_path is specified, assume we are writing out to DBI connection
    if (!is.null(output_path)) {

      # either use existing connection
      if (inherits(output_path, 'DBIConnection')) {
        conn <- output_path
      } else {
        # or default if no connection specified uses sqlite
        conn <- DBI::dbConnect(RSQLite::SQLite(), output_path)
      }

      # write tables
      res <- sapply(names(x5), function(x) try(DBI::dbWriteTable(conn, x, x5[[x]],
                                                                 overwrite = overwrite,
                                                                 append = append)))

      # check and notify on errors
      bad.idx <- which(sapply(res, inherits, 'try-error'))

      if (length(bad.idx)) {
        sapply(bad.idx, function(i) message('failed to write table `', names(x5)[i], '`'))
      }
    }
    return(invisible(x5))
  } else {
    message("currently only /tabular/*.txt file (MS Access-style) exports are supported")
  }
  invisible(list())
}
