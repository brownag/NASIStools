#' Create Worksheet Import Mapping XML
#' 
#' This function generates XML mapping the columns of an "import worksheet" (Excel Spreadsheet) to NASIS tables and columns for use in the `wsimportmap` table.
#' 
#' @param .data a _data.frame_ or _list_ with names using format `[table name].[column_logical_name]`, and elements containing either a) the Excel spreadsheet column letter (e.g. `A`, `B`, `C`), b) `"lookup"` for attributes that refer to a foreign key, c) any other value is assumed to be a constant for all entries.
#' @return _character_ string containing XML mapping
#' @export
#' 
#' @examples
#' .data <- data.frame(
#'   site.user_site_id = "A",
#'   siteobs.observation_date = "B",
#'   siteobs.observation_date_kind = "C",
#'   siteobs.data_collector = "D",
#'   pedon.describers_name = "E"
#' )
# create_import_mapping <- function(.data, description = "") {
#   x <- do.call('rbind', strsplit(colnames(.data), ".", fixed = TRUE))
#   tn <- x[,1]
#   cn <- x[,2]
#   utn <- unique(tn)
#   sprintf(
#     '<?xml version="1.0" encoding="UTF-8"?>
#         <spreadsheet>
#         	%s
#         	<table name="site" maxrows="2000" rowstart="4">
#         		<column name="user_site_id" column="A"/>
#         	</table>
#           <table name="siteobs" maxrows="1" rowstart="4">
#         		<column name="observation_date" column="B"/>
#         		<column name="observation_date_kind" column="C"/>
#         		<column name="data_collector" column="D"/>
#         	</table>
#           <table name="pedon" maxrows="1" rowstart="4">
#         		<column name="describers_name" column="E"/>
#         	</table>
#         </spreadsheet>',
#     sprintf('<description>%s</description>', description),
#     sprintf('<table name="%s" maxrows="2000" rowstart="4">
#           		<column name="user_site_id" column="A"/>
#           	</table>', utn)
#   )
# }
# 
