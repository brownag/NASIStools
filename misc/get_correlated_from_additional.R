get_correlated_from_additional <- function(dmuiid) {
  x <- dbQueryNASIS(NASIS(), sprintf("SELECT * FROM legend
                                 INNER JOIN lmapunit ON legend.liid = lmapunit.liidref
                                 INNER JOIN mapunit ON lmapunit.muiidref = mapunit.muiid
                                 INNER JOIN correlation ON mapunit.muiid = correlation.muiidref
                                 WHERE dmuiidref IN %s
                                 ", format_SQL_in_statement(dmuiid))) |> uncode()
  xx <- split(x, paste0(x$dmuiidref))
  res <- do.call('rbind', lapply(names(xx), function(y) {
    res2 <- subset(xx[[y]], xx[[y]]$mustatus == "correlated", 
                   select = c('liidref','muiidref','muname','nationalmusym'))
    if (nrow(res2) == 0) {
      res2 <- res2[0,][1,]
    }
    res2$dmuiid <- y
    res2
  }))
  rownames(res) <- NULL
  res
}

get_correlated_from_additional(c(292893L, 292903L, 292916L, 292917L, 292955L, 292956L, 299158L, 
                                 393246L, 393247L, 393248L, 293492L, 293557L, 293592L, 293593L, 
                                 293762L, 293763L, 293821L, 293822L))
