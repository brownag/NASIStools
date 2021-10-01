create_import_template <- function(.data,
                                   file,
                                   template_name,
                                   columns,
                                   template_version = "1.0") {

  stopifnot(is.character(template_name))
  stopifnot(is.character(columns))
  stopifnot(length(columns) > 1)
  x <- c(paste0(c(template_name, shQuote(template_version), rep("", length(columns) - 2)), collapse = ","),
         paste0(rep(",", length(columns)), collapse = ""),
         paste0(shQuote(gsub("_", " ", columns)), collapse = ","),
         paste0(apply(.data[, columns, drop = FALSE], 1, paste0, collapse = ",")))
  writeLines(x, file)
}

create_ESD_ecosites_import <- function(file, coiids, ecositeids) {
  create_import_template(
    data.frame(coiid = coiids,
               `Ecosite ID` = ecositeids),
    file = file,
    template_name = "ESD Ecosites",
    columns = c("coiid", "Ecosite_ID")
  )
}

create_ESD_notes_import <- function(file, coiids, author, notes) {
  create_import_template(
    data.frame(
      coiid = coiids,
      author = author,
      note = notes
    ),
    file = file,
    template_name = "ESDEditNote",
    columns = c("coiid", "author", "note")
  )
}
