#' Create NASIS XLSX Import Files
#'
#' @param .data a data.frame containing source data
#' @param file output file name
#' @param template_name template name
#' @param sheet XLSX sheet name
#' @param columns columns in template
#' @param template_version template version; default: `"1.0"`
#' @param delimeter used internally for creating matrix representation for writing to file. Default: `"<delimiter|||>"`
#' @details Column names containing `"_"` are converted to `" "`
#' @return writes XLSX or CSV file
#' @export
#'
#' @importFrom utils write.csv
create_import_template <- function(.data,
                                   file,
                                   template_name,
                                   columns,
                                   sheet,
                                   template_version = "1.0",
                                   delimeter = "<delimiter|||>") {

  stopifnot(is.character(template_name))
  stopifnot(is.character(columns))
  stopifnot(length(columns) > 1)

  stopifnot(endsWith(file, ".csv") || endsWith(file, ".xlsx"))

  as_xlsx <- endsWith(file, ".xlsx")
  
  x <- c(paste0(c(template_name, template_version, rep("", length(columns) - 1)),
                collapse = delimeter),
         paste0(rep(delimeter, length(columns)), collapse = ""),
         paste0(paste0(columns, collapse = delimeter)),
         paste0(apply(as.matrix(.data)[, colnames(.data) %in% columns, drop = FALSE], 
                      MARGIN = 1, FUN = paste0, collapse = delimeter), delimeter))
  m <- lapply(x, function(x) strsplit(x, split = delimeter, fixed = TRUE)[[1]])
  mat <- do.call('rbind', m)
  mat[is.na(mat)] <- ""
  
  if (as_xlsx) {

    if (!requireNamespace("openxlsx"))
      stop("The openxlsx package is required to write XLSX files", call. = FALSE)

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, sheetName = sheet)

    lapply(1:ncol(mat), function(i) {
      openxlsx::writeData(wb, sheet = sheet, x = trimws(mat[,i]), xy = c(i, 1))
    })
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  } else utils::write.csv(.data, file, row.names = FALSE, quote = TRUE)

}

#' @param .data Optional: data.frame containing NASIS column physical or logical names to be used to populate the template. Default is an empty template.
#' @param coltype Either `"physical"` for physical column names, or `"logical"` (default) for logical column names (default for worksheet import maps).
#' @param ... Additional arguments passed to `create_import_template()`
#' @rdname create_import_template
#' @export
create_import_template_from_mapping <- function(.data = NULL,
                                                coltype = c("logical", "physical"),
                                                file,
                                                template_name,
                                                sheet,
                                                ...) {
  .SD <- NULL
  coltype <- match.arg(tolower(coltype[1]), choices = c("logical", "physical"))
  colnm <- paste0(coltype, "_name")
  
  y <- read_import_mapping(sheet)
  y <- y[, .SD[1, ], by = "column"]
  y <- y[order(nchar(y$column), y$column), ]
  
  if (is.null(.data)) {
    .data <- as.data.frame(sapply(y[[colnm]], \(x) character()))
  }
  
  if (any(!y[[colnm]] %in% names(.data)))
    stop("the following columns are required:\n",
         paste0(y[[colnm]][!y[[colnm]] %in% names(.data)], collapse = ", "), call. = FALSE)
  
  create_import_template(.data, 
                         columns = y[[colnm]],
                         file = file,
                         template_name = template_name,
                         sheet = sheet, ...)
  
}


#' Create Ecosite / Ecosite Note XLSX NASIS Import Files
#'
#' @param file output file name (either .XLSX or .CSV)
#' @param coiids vector of component IDs
#' @param ecositeids vector of ecological site IDs
#' @param author author of note
#' @param notes note content
#' @param template a `sprintf()`-style format string up to 8192 bytes in length
#' @param ... values to be passed into `template`. Only logical, integer, real and character vectors are supported.
#' @param sheet Default XLSX sheet name `"ESDList"`
#'
#' @return writes XLSX or CSV file
#' @export
#' @rdname ecosite-import
#' @importFrom stats aggregate
#' @examples
#'
#' library(soilDB)
#'
#' create_ESD_ecosites_import("test_esd.xlsx", 2770865, "F018XI205CA")
#'
#' if (soilDB::local_NASIS_defined()) {
#'   esdnotes <- create_note_from_ESD_ecosites("test_esd.xlsx", "Assigned %s %s")$note
#'
#'   esdnotes
#'
#'   create_ESD_notes_import("test_esd_note.xlsx",
#'                           coiids = 2770865,
#'                           author = "Andrew Brown",
#'                           notes = esdnotes)
#'   unlink('test_esd_note.xlsx')
#' }
#' unlink('test_esd.xlsx')
create_ESD_ecosites_import <- function(file, coiids, ecositeids) {

  if (any(aggregate(ecositeids, list(coiids), function(x) length(unique(x)))$x > 1)) {
    warning("Some component IDs have more than one unique ecosite assigned; this can happen if different ecosites are assigned to a component that exists on multiple legends. Note that the relationship between coiid and unique ecosite IDs should be 1:1.", call. = FALSE)
  }

  create_import_template(
    unique(data.frame(coiid = coiids,
                      Ecosite_ID = ecositeids)),
    file = file,
    template_name = "ESD Ecosites",
    columns = c("coiid", "Ecosite_ID"),
    sheet = "ESDList"
  )
}

#' @export
#' @rdname ecosite-import
create_ESD_notes_import <- function(file, coiids, author, notes) {
  create_import_template(
    unique(data.frame(
      coiid = coiids,
      author = author,
      note = notes
    )),
    file = file,
    template_name = "ESDEditNote",
    columns = c("coiid", "author", "note"),
    sheet = "ESDnote"
  )
}

#' @export
#' @rdname ecosite-import
create_note_from_ESD_ecosites <- function(file, template, ..., sheet = "ESDList") {
  stopifnot(requireNamespace("openxlsx"))
  x <- openxlsx::read.xlsx(file, sheet = sheet)
  x <- x[3:nrow(x),]
  colnames(x) <- c("coiid","Ecosite ID")
  ecositenames <- ecositeid_to_name(x$`Ecosite ID`)
  x$note <- sprintf(template, x$`Ecosite ID`, ecositenames, ...)
  x
}
