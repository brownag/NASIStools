library(NASIStools)
library(soilDB)
f <- fetchNASIS()
f$texcl <- aqp::ssc_to_texcl(f$sand, f$clay)
f
aqp::plot(f, color = "clay")

f$lieutex[grepl("O", f$hzname)] <- "SPM"
f$lieutex[grepl("Cr|R", f$hzname)] <- "BR"
f$lieutex
d <- data.frame(
  phiid = f$phiid,
  texcl = f$texcl,
  lieutex = f$lieutex,
  texmod = gsub("([^\\-]*)-.*|[^\\-]*", "\\1", f$texture),
  fragkind = "",
  fragvol = "",
  fraground = "",
  fragshp = "",
  fragsize_h = "",
  fragsize_l = "",
  fraghard = ""
)
d[] <- lapply(d, as.character)
d[is.na(d)] <- " "
create_import_template(d, file = "phtexture.xlsx", 
                       template_version = "0.001",
                       template_name = "SWRPedons",
                       columns = c("phiid", "texcl", "lieutex", "texmod", "fragkind", "fragvol", "fraground", "fragshp", "fragsize_h", "fragsize_l", "fraghard"),
                       sheet = "phtexture")
f <- fetchNASIS()
f$texcl[!is.na(f$lieutex)] <- f$lieutex[!is.na(f$lieutex)]
aqp::plotSPC(f, color = "texcl", label = 'pedon_id')
