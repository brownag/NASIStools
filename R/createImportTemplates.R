#' Create NASIS import template
#'
#' @param .data a data.frame containing source data
#' @param file output file name
#' @param template_name template name
#' @param columns columns in template
#' @param template_version template version; default: `"1.0"`
#' @details Column names containing `"_"` are converted to `" "`
#' @return writes CSV file
#' @export
create_import_template <- function(.data,
                                   file,
                                   template_name,
                                   columns,
                                   template_version = "1.0") {

  stopifnot(is.character(template_name))
  stopifnot(is.character(columns))
  stopifnot(length(columns) > 1)
  x <- c(paste0(c(template_name, paste0("=", shQuote(template_version)),
                  rep("", length(columns) - 2)), collapse = ","),
         paste0(rep(",", length(columns)), collapse = ""),
         paste0(shQuote(gsub("_", " ", columns)), collapse = ","),
         paste0(apply(.data[, columns, drop = FALSE], 1, paste0, collapse = ",")))
  writeLines(x, file)
}

#' Create Ecosite / Ecosite Note Import Files
#'
#' @param file output file name
#' @param coiids vector of component IDs
#' @param ecositeids vector of ecological site IDs
#' @param author author of note
#' @param notes note content
#'
#' @return writes CSV file
#' @export
#' @rdname ecosite-import
create_ESD_ecosites_import <- function(file, coiids, ecositeids) {
  create_import_template(
    unique(data.frame(coiid = coiids,
                      Ecosite_ID = ecositeids)),
    file = file,
    template_name = "ESD Ecosites",
    columns = c("coiid", "Ecosite_ID")
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
    columns = c("coiid", "author", "note")
  )
}
