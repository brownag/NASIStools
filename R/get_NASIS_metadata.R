# Get NASIS metadata entries for specific domains or choices
#
# @param x Vector to match in NASIS metadata
# @param what Column to match `x` against. Default "ColumnPhysicalName"; alternate options include `"DomainID"`, `"DomainName"`, `"DomainRanked"`, `"DisplayLabel"`, `"ChoiceSequence"`, `"ChoiceValue"`, `"ChoiceName"`, `"ChoiceLabel"`, `"ChoiceObsolete"`, `"ChoiceDescription"`, `"ColumnLogicalName"`
# @param dsn passed to `soilDB::get_NASIS_metadata()` for alternate NASIS database connection types (path to SQLite file or open DBIConnection)
#
# @return a data.frame containing selected NASIS metadata sorted first on `DomainID` and then on `ChoiceSequence`
# @examples
# # get_NASIS_metadata("texcl")
#' @importFrom soilDB get_NASIS_metadata
get_NASIS_column_metadata <- function(x, 
                                      what = "ColumnPhysicalName",
                                      dsn = NULL) {
  .Deprecated("get_NASIS_column_metadata", "soilDB")
  metadata <- soilDB::get_NASIS_metadata(dsn = dsn)
  mds <- metadata[metadata[[what]] %in% x, ] 
  mds <- mds[order(mds$DomainID, mds$ChoiceSequence), ]
  mds
}
