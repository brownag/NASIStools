# conversion functions, using your NASIS local database!

.the_lut <- function(x, from = 'musym', areasymbol = NULL, query_string = FALSE) {
  q <- sprintf("SELECT areasymbol, musym, nationalmusym, lmapunitiid, muiid, muname, dmuiid
     FROM mapunit
     INNER JOIN correlation ON correlation.muiidref = mapunit.muiid
     INNER JOIN datamapunit ON correlation.dmuiidref = datamapunit.dmuiid AND repdmu = 1
     INNER JOIN lmapunit ON lmapunit.muiidref = mapunit.muiid
     INNER JOIN legend ON legend.liid = lmapunit.liidref
     INNER JOIN area ON area.areaiid = legend.areaiidref
     %s %s",
    ifelse(!is.null(x), paste0("WHERE ", from, " IN ",
                               soilDB::format_SQL_in_statement(x)), ""),
    ifelse(!is.null(x) && !is.null(areasymbol), paste0("AND areasymbol IN ",
                                        soilDB::format_SQL_in_statement(areasymbol)), ""))
  if (query_string) return(q)
  soilDB::dbQueryNASIS(soilDB::NASIS(), q)
}

.do_lut <- function(x, from, to, verbose = FALSE, ...) {
  lut <- .the_lut(x, from = from, ...)
  res <- lut[, to, drop = FALSE][match(x, lut[[from]]),]
  if (verbose) sapply(res, cat, "\n")
  invisible(res)
}

musym_to_nmusym <- function(x, from = 'musym', to = "nationalmusym", ...) {
  .do_lut(x, from, to, ...)
}

musym_to_muiid <- function(x, from = 'musym', to = "muiid", ...) {
  .do_lut(x, from, to, ...)
}

musym_to_dmuiid <- function(x, from = 'musym', to = "dmuiid", ...) {
  .do_lut(x, from, to, ...)
}

musym_to_muname <- function(x, from = 'musym', to = "muname", ...) {
  .do_lut(x, from, to, ...)
}

musym_to_all <- function(x, from = 'musym', to = c("areasymbol", "musym", "nationalmusym", "lmapunitiid", "muiid", "muname", "dmuiid"), ...) {
  as.data.frame(.do_lut(x, from, to, ...))
}
#
# nusyms <- c("7103", "7102", "8173", "7091", "7092", "8172", "8173", "7103",
#   "7103", "8171", "8172", "8173", "8171", "8172", "8173", "8172",
#   "7102", "8173", "MaG2", "8162", "8172")
# # write.csv(musym_to_nmusym(musyms), file = "test.csv")
# oldsyms <- c("BrF2", "BrF2", "BrF2", "BrF2", "BrF2", "BrF2", "BrF3", "BrG2",
#   "BeF", "JbD2", "JbE2", "JbF2", "JcD2", "JcE2", "JcF2", "JcF2",
#   "JcF2", "JdG2", "JdG2", "JdG2", "JeF2")
# old731 <- c("JbDma", "BdDma", "BdEma", "JbFma", "AkFma", "TbFma", "BrFma",
#             "JcDma", "BeFma")
# new731 <- c(7087L, 7092L, 7102L, 7103L)
# musym_to_nmusym(nusyms, verbose=T)
# musym_to_muiid(nusyms, verbose=T)
# musym_to_dmuiid(nusyms, verbose=T)
# musym_to_muname(nusyms, verbose=T)
# musym_to_nmusym(oldsyms, verbose=T)
# musym_to_muiid(oldsyms, verbose=T)
# musym_to_dmuiid(oldsyms, verbose=T)
# musym_to_muname(oldsyms, verbose=T)
#
#   musym_to_nmusym(old731, verbose=T)
# musym_to_muiid(old731, verbose=T)
# musym_to_dmuiid(old731, verbose=T)
# musym_to_muname(old731, verbose=T)
#
# musym_to_nmusym(new731, verbose=T)
# musym_to_muiid(new731, verbose=T)
# musym_to_dmuiid(new731, verbose=T)
# musym_to_muname(new731, verbose=T)
#
# new750 <- c(8172L, 8173L, 8162L, 7102L, 7103L)
# musym_to_nmusym(new750, verbose=T)
# musym_to_muiid(new750, verbose=T)
# musym_to_dmuiid(new750, verbose=T)
# musym_to_muname(new750, verbose=T)
#
# x <- dput(read.table(text= "BmGma
# BrFma
# JcFma")$V1)
# asym = "CA750"
# musym_to_nmusym(x, areasymbol = asym, verbose=T)
# musym_to_muiid(x, areasymbol = asym, verbose=T)
# musym_to_dmuiid(x, areasymbol = asym, verbose=T)
# musym_to_muname(x, areasymbol = asym, verbose=T)
# y <- c("CA750136",
#   "CA750JcFma",
#   "CA750142",
#   "CA750MaFma",
#   "CA750BrFma")
# foo<- sapply(gsub("[A-Z]{2}[0-9]{3}(.*)", "\\1", y), cat, "\n")
#
#
#
# y <- c("AnE", "TbF2", "BkE2", "RcG", "AnG2", "BrF2", "AkF2", "AmG3",
#   "BeF", "BdD", "TaD2", "AhE2", "7088", "AhD", "7086", "MbG3",
#   "BrF3", "BeD", "BdE", "WaF", "DaE", "DbG", "W", "BlF", "BfG",
#   "HaG", "LdC", "JeF2", "Rb", "McE", "MbH2", "7079", "MaF2", "BrG2",
#   "JcD2", "7076", "DaD")
# musym_to_all(y, verbose=T)
