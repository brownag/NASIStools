#' Create a NASIS Flat File for importing Pedon Records
#'
#' @param tables default `NULL` creates an empty template; alternately a list containing data.frames with target table names and new data to write to file.
#' @param filename path to output file containing templates for importing each table associated with the NASIS Pedon object
#' @param dsn optional path to SQLite Database or an open DBIConnection to a database containing NASIS pedon tables (not yet implemented)
#' @param overwrite default `FALSE` prevents overwriting `filename` if it exists
#'
#' @return a character vector (invisibly) containing the lines/content written out to `filename`
#' @export
#' @author Jay Skovlin, Andrew G. Brown
#' @importFrom utils write.table
create_PedonPC_NASIS_import <- function(tables = NULL, filename, dsn = NULL, overwrite = FALSE) {

  if (file.exists(filename)) {
    if (overwrite) {
      cat("", file = filename, append = FALSE)
    } else stop(sprintf("File (%s) already exists. Set overwrite=TRUE to overwrite contents.", filename),
                call. = FALSE)
  }

  # NASIS_table_column_keys <- read.table(file = 'E:/workspace/USFSMigration/nasis_pedon_table_columns.txt',
  #                                       sep = ',', header = TRUE, stringsAsFactors = FALSE)
  NASIS_table_column_keys <- NULL
  load(system.file("data/NASIS_table_column_keys.rda", package = "soilDB"))
  pedon_tables <- c("site", "siteaoverlap", "sitebedrock", "siteecositehistory", 
                    "sitegeomordesc", "sitepm", "sitetext", "siteobs", "siteerosionacc", 
                    #"siteexistveg", 
                    "siteobstext", "sitesoilmoist", "sitesoiltemp", 
                    "sitesurffrags", "transect", "transectestcomposition", "transecttext", 
                    "siteassoc", "siteassocsite", "siteassocsoi", "siteassoctext", 
                    "pedon", "pediagfeatures", "pefmp", "pehydricfieldindicator", 
                    "pepenetrationresistance", "perestrictions", "pesoilstability", 
                    "petaxhistory", "petext", "peinfiltrationsummary", "petaxhistfmmin", 
                    "petxhistfmother", "petaxhistmoistcl", "peinfiltrationch", "peinfiltrationfh", 
                    "peinfiltrationchdata", "peinfiltrationfhdata", "phorizon", "phcemagent", 
                    "phcolor", "phconcs", "phcracks", "phdesgnsuffix", "phfeatures", 
                    "phfmp", "phfrags", "phhuarts", "phmottles", "phpvsf", "phpores", 
                    "phrdxfeatures", "phroots", "phsample", "phstructure", "phtext", 
                    "phtexture", "phdb", "phdbcompliantcavity", "phdbcore", "phdbscoop", 
                    "phdbcorereading", "phdbscoopreading", "phconccolor", "phfeatcolor", 
                    "phpvsfcolor", "phredoxfcolor", "phtexturemod")
  NASIS_table_column_keys <- NASIS_table_column_keys[sort(which(NASIS_table_column_keys$table %in% pedon_tables)),]

  for (i in 1:nrow(NASIS_table_column_keys)) {
    # build header
    table_str <- paste("@begin ", NASIS_table_column_keys$table[i], sep = '')

    #get colnames for each table
    col_str <- paste0(NASIS_table_column_keys$column[i], '\n')

    # build data table name
    table.name <- NASIS_table_column_keys$table[i]

    # build full header with colnames and write to file
    header <- paste(table_str, col_str, sep = '\n')
    cat(header, file = filename, append = TRUE)

    if (!is.null(tables[[table.name]])) {
      tcols <- strsplit(NASIS_table_column_keys$column[i], ",")[[1]]
      ttemp <- data.frame(lapply(tcols, function(x) rep(NA, nrow(tables[[table.name]]))))
      colnames(ttemp) <- tcols
      update <- names(ttemp)[names(ttemp) %in% names(tables[[table.name]])]
      ttemp[update] <- tables[[table.name]][update]
      write.table(soilDB::code(ttemp), file = filename, na = '', sep = ',',
                  row.names = FALSE, col.names = FALSE, append = TRUE)
    }

    # build footer and write to file
    footer <- "@end\n"
    cat(footer, file = filename, append = TRUE)
  }
  invisible(readLines(filename))
}
