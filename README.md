# NASIStools

<!-- badges: start -->
[![R-CMD-check](https://github.com/brownag/NASIStools/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/brownag/NASIStools/actions/workflows/R-CMD-check.yml)
[![NASIStools Manual](https://img.shields.io/badge/docs-HTML-informational)](https://brownag.github.io/NASIStools/)
<!-- badges: end -->

The goal of NASIStools is to provide methods managing NASIS data, SSURGO exports, official metadata as well as to provide ports of legacy code (such as MS Access Macros) to open source alternatives. Use cases primarily relate to data model development, QA/QC of provisional data and inspection of published products. 

This is part of a broader goal to compartmentalize low-level procedures relevant to internal (USDA) usage. Isolating lesser-used, more specific functions allows for more comprehensive testing and opportunities to focus development of the soilDB package.

## Installation

Install the package off GitHub:

``` r
# get dependencies
install.packages(c('remotes', 'data.table', 'DBI', 'RSQLite', 'soilDB'))

# get package using remotes
remotes::install_github("brownag/NASIStools", dependencies=FALSE)
```

## Example

Here is how to query the "reasons" for a particular interpretation `mrulename` rating for the records stored in a SSURGO SQLite database derived from a custom NASIS export.

``` r
library(NASIStools)

# create a SQLite file database based on the .txt files contained in a SSURGO export
createSSURGO("C:/path/to/ssurgo/tabular", "ssurgo.sqlite")

# specify a rule name
mrulename <-  "FOR - Mechanical Site Preparation (Surface)"

# create a DBIConnection to the SQLite database
dsn <- DBI::dbConnect(RSQLite::SQLite(), "ssurgo.sqlite")

# perform a query with NASIStools
result <- get_SSURGO_interp_reasons_by_mrulename(dsn, mrulename)
```

```r
library(soilDB)
library(data.table)

mrulename <-  "WMS - Pond Reservoir Area" 

# "new" tabular data from a local export
result <- get_SSURGO_interp_reasons_by_mrulename(dsn, mrulename)

# "old" tabular data queried from SDA
result_ssurgo <- get_SDA_interpretation(mrulename,
                                        method = "NONE",
                                        areasymbols = c("CA630","CA649"))
                                        
# assemble tables of mukey, compname, rating/class and reason
x1 <- data.table(mukey = result$mukey,
                 compname = result$compname,
                 rating_new = result$interphr,
                 rating_class = result$interphrc,
                 rating_new_reason = result$Reason)

x2 <- data.table(mukey = result_ssurgo$mukey,
                 compname = result_ssurgo$compname,
                 rating_old = result_ssurgo$rating,
                 rating_class = result_ssurgo$class,
                 rating_old_reason = result$reason_WMSPondReservoirArea)

combined_result <- x2[x1, on = c("mukey","compname")]

combined_result$CHECK <- round(combined_result$rating_new, 2) == round(combined_result$rating_old, 2)
```

