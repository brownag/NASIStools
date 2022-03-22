#' Get NASIS metadata entries for specific domains or choices
#'
#' @param x Vector to match in NASIS metadata
#' @param what Column to match `x` against. Default "ColumnPhysicalName"; alternate options include `"DomainID"`, `"DomainName"`, `"DomainRanked"`, `"DisplayLabel"`, `"ChoiceSequence"`, `"ChoiceValue"`, `"ChoiceName"`, `"ChoiceLabel"`, `"ChoiceObsolete"`, `"ChoiceDescription"`, `"ColumnLogicalName"`
#' @param dsn passed to soilDB:::.get_NASIS_metadata() for alternate NASIS database connection types (path to SQLite file or open DBIConnection)
#'
#' @return a data.frame containing selected NASIS metadata sorted first on `DomainID` and then on `ChoiceSequence`
#' @export
#'
#' @examples
#' get_NASIS_metadata("texcl")
get_NASIS_metadata <- function(x, 
                               what = "ColumnPhysicalName",
                               dsn = NULL) {
  metadata <- soilDB:::.get_NASIS_metadata(dsn = dsn)
  if (nrow(metadata) == 0) {
    load(system.file("data/metadata.rda", package = "soilDB"))
  }
  mds <- metadata[metadata[[what]] %in% x, ] 
  mds <- mds[order(mds$DomainID, mds$ChoiceSequence), ]
  mds
}
