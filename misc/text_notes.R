# textcheckr
library(DBI)
library(soilDB)
library(magrittr)

# prepare text note table snapshot
get_NASIS_textnotes <- function(SS = TRUE, tables = c("cotext",
                                                      "dmutext",
                                                      "dmucerthistory",
                                                      "muhistory",
                                                      "mutext",
                                                      "ltext"),
                                dsn = NULL) {

  # selected set only?
  qtbls <- switch(SS, paste0(tables, "_View_1"), tables)
  txt <- soilDB::createStaticNASIS(tables = qtbls, new_names = NULL, dsn = dsn, output_path = NULL, SS = SS)
  txt <- lapply(txt, soilDB::uncode)
  names(txt) <- tables
  txt
}

checksubset <- function(x, idvar, ...) {
  subset(x, ...)[[idvar]]
}

# get_componenttolegend <- function(SS = TRUE, rep_only = TRUE) {
#   q <- "SELECT * FROM legend_View_1
#         INNER JOIN lmapunit_View_1 ON legend_View_1.liid = lmapunit_View_1.liidref
#         INNER JOIN mapunit_View_1 ON lmapunit_View_1.muiidref = mapunit_View_1.muiid
#         INNER JOIN correlation_View_1 ON mapunit_View_1.muiid = correlation_View_1.muiidref
#         INNER JOIN datamapunit_View_1 ON correlation_View_1.dmuiidref = datamapunit_View_1.dmuiid
#         INNER JOIN component_View_1 ON datamapunit_View_1.dmuiid = component_View_1.dmuiidref"
#   if (!SS) {
#     q <- gsub("_View_1", "", q)
#   }
#   ch <- soilDB::uncode(soilDB::dbQueryNASIS(soilDB::NASIS(), q))
#   if (rep_only){
#     return(subset(ch, repdmu & compkind != "miscellaneous area"))
#   }
#   ch
# }

check_required_notes <- function(SS = TRUE, rep_only=TRUE) {
  ch <- get_componenttolegend(SS = SS, rep_only = rep_only)
  # INNER JOIN chorizon_View_1 ON component_View_1.coiid = chorizon_View_1.coiidref")

  # soilDB::NASIS() %>%
  #  DBI::dbListTables() %>%
  #  subset(grepl(pattern = "text", .))

  # check representative DMUs
  ch <- subset(ch, repdmu)
  ch <- subset(ch, compkind != "miscellaneous area")

  # check SWR and nationally mandated text notes for component data

  # identify coiid, dmuiid, muiid, liid of interest
  liids <- unique(ch$liid)
  liids <- unique(ch$liid)
  muiids <- unique(ch$muiid)
  dmuiids <- unique(ch$dmuiid)
  coiids <- unique(ch$coiid)
  # chiids <- unique(ch$chiid)

  # create text note table snapshot
  txt <- get_NASIS_textnotes(SS = SS)

  # TODO: generalize definitions andcollect metadata about existing notes
  list(
    # component text (cotext) expect 1:1 with coiid
    cotext = list(
      # calculated general soil description
      gsd = coiids[!coiids %in% checksubset(txt[["cotext"]], idvar = "coiidref",
                                            comptextkind == "nontechnical description" & textcat == "GENSOIL")],
      # ecosite notes
      ecosite = coiids[!coiids %in% checksubset(txt[["cotext"]], idvar = "coiidref",
                                                comptextkind == "edit notes" & textcat == "ESD")],
      # edit notes
      edits = coiids[!coiids %in% checksubset(txt[["cotext"]], idvar = "coiidref",
                                              comptextkind == "edit notes" & textcat == "edits")]
    ),
    # data mapunit text (dmutext) expect 1:1 with dmuiid
    dmutext = list(
      dmucreation = dmuiids[!dmuiids %in% checksubset(txt[["dmutext"]], idvar = "dmuiidref",
                                                      dmutextkind == "miscellaneous notes" & textcat == "creation")],
      dmuinterpretation = dmuiids[!dmuiids %in% checksubset(txt[["dmutext"]], idvar = "dmuiidref",
                                                            dmutextkind == "miscellaneous notes" & textcat == "interpretations")],
      dmuspot = dmuiids[!dmuiids %in% checksubset(txt[["dmutext"]], idvar = "dmuiidref",
                                                  dmutextkind == "miscellaneous notes" & textcat == "spot symbols")]
    ),
    dmucerthistory = list(
      # data mapunit certification history (dmucerthistory) expect 1:1 with dmuiid
      qccert = dmuiids[!dmuiids %in% checksubset(txt[["dmucerthistory"]], idvar = "dmuiidref",
                                                 certificationkind == "quality control" & dmucertstat == "certified, all components")],
      qacert = dmuiids[!dmuiids %in% checksubset(txt[["dmucerthistory"]], idvar = "dmuiidref",
                                                 certificationkind == "quality assurance" & dmucertstat == "certified, all components")]
    ),
    muhistory = # mapunit history (muhistory) expect 1:1 with muiid
      list(
        corramend = muiids[!muiids %in% checksubset(txt[["muhistory"]], idvar = "muiidref",
                                corkind == "join statement" & corevent == "correlation amendment")],
        finalcorr = muiids[!muiids %in% checksubset(txt[["muhistory"]], idvar = "muiidref",
                                corkind == "notes to accompany" & corevent == "final correlation")]
      ),
    # mapunit text (mutext) expect 1:1 with muiid
    mutext = list(
      mucreation = muiids[!muiids %in% checksubset(txt[["mutext"]], idvar = "muiidref",
                               mapunittextkind == "miscellaneous notes" & textcat == "creation")],
      muupdate = muiids[!muiids %in% checksubset(txt[["mutext"]], idvar = "muiidref",
                             mapunittextkind == "miscellaneous notes" & textcat == "update summary")]
    ),
    # legend text (ltext) expect 1:1 with liid
    ltext = list(
      legendamend = liids[!liids %in% checksubset(txt[["ltext"]], idvar = "liidref",
                                legendtextkind == "correlation note" &
                                  textcat == "amendment" &
                                  textsubcat == substring(Sys.Date(), 1, 4))]
    )
  )
}

get_SWR_text_notes <- function(SS = TRUE,
                               rep_only = TRUE,
                               verbose = TRUE) {
    # check required notes
    notereq <- check_required_notes(SS = SS, rep_only = rep_only)
    res <- lapply(names(notereq), function(x) {
      sapply(sapply(notereq[[x]], length), function(n)
        paste0(n, " notes in table '", x, "'"))
    })
    sapply(seq_along(res), function(i)
      sapply(names(res[[i]]), function(x)
        if (verbose)
          message(x, " -- missing ", res[[i]][[x]])))
  }

# component master list
component <- soilDB::get_component_data_from_NASIS_db(SS = TRUE)
res2 <- get_SWR_text_notes()
# fix required notes

# ECOLOGICAL SITE NOTES
# get assigned ecosite info
esassigned <- soilDB::uncode(soilDB::dbQueryNASIS(soilDB::NASIS(),
                                           paste0("SELECT * FROM component_View_1
                                                   LEFT JOIN coecosite_View_1
                                                   ON component_View_1.coiid = coecosite_View_1.coiidref
                                                   LEFT JOIN ecologicalsite
                                                   ON coecosite_View_1.ecositeiidref = ecologicalsite.ecositeiid
                                                   AND coiidref IN ",
                                                  format_SQL_in_statement(notereq$cotext$ecosite))))
# misc areas removed explicitly, after implicitly including them with LEFT join
esassigned <- subset(esassigned, compkind != "miscellaneous area")

# now check what ones need notes, group by ecositenm
esgroups <- split(esassigned, esassigned$ecositenm)
lapply(esgroups, function(x) data.frame(unique(component[component$coiid %in% x$coiid, c("musym","compname")]),
                                        ecositenm = unique(x$ecositenm)))

# the following coiid do not have an ecological site assigned
noes <- notereq$cotext$ecosite[!notereq$cotext$ecosite %in% esassigned$coiid]
noescomp <- component[component$coiid %in% noes,]

table(noescomp$compkind)

# check edit notes
# the following coiid do not have an edit note
noed <- notereq$cotext$edits
noedcomp <- component[component$coiid %in% noed,]

table(noedcomp$compkind)


