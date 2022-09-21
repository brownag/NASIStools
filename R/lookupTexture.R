#' Lookup Soil Texture Abbreviation Codes or Names
#'
#' @param x a vector of soil texture/texture modifier abbreviation codes or names
#' @param what either `"names"` (default) or `"codes"`. The opposite of what `x` is.
#' @param dsn passed to `get_NASIS_metadata()`
#' @return a vector of soil texture abbreviation codes or names depending on whether input values are matched in the lookup table and the value of `what`. Unmatched values in `x` return `NA`.
#' @export
#' @importFrom soilDB get_NASIS_column_metadata
#' @examples
#' x <- lookupTexture(c("loam", "bar", "sandy loam"), what = "codes")
#' x
#' lookupTexture(x, what = "names")
#' 
#' lookupTextureModifier("extremely bouldery")
lookupTexture <- function(x, what = "names", dsn = NULL) {
  md <- soilDB::get_NASIS_column_metadata("texcl", dsn = dsn)
  stl <- md$ChoiceName
  stn <- md$ChoiceLabel
  if (what == "codes") {
    return(stl[match(tolower(x), tolower(stn))])
  } else return(stn[match(tolower(x), tolower(stl))])
}

#' @export
#' @rdname lookupTexture
lookupTextureModifier <- function(x, what = "names", dsn = NULL) {
  md <- soilDB::get_NASIS_column_metadata("texmod", dsn = dsn)
  stl <- md$ChoiceName
  stn <- md$ChoiceLabel
  if (what == "codes") {
    return(stl[match(tolower(x), stn)])
  } else return(stn[match(tolower(x), stl)])
}
