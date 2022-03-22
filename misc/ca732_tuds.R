library(NASIStools)

# read SPCs containing TUD data
x1 <- readRDS("E:/CA732/CA760_TUDs/CA760_TUD_SPC.rds")
x2 <- readRDS("E:/CA732/CA740_TUDs/CA740_TUD_SPC.rds")
x <- aqp::combine(x1, x2)

# convert names to abbreviations that can be encoded to NASIS domain
x$texcl <- lookupTexture(x$texture_class, "codes")
x$texmod <- lookupTextureModifier(x$cf_class, "codes")

# unique numeric key identifying 1:1:1 site/pedon/siteobs  
x$.pID <- seq_along(aqp::profile_id(x))
x$.pID_hz <- aqp::denormalize(x, ".pID")

# tables list
tables <- list()

# soilDB::get_NASIS_table_key_by_name("phorizon")

### tables 1:1 with SPC structures only need remapping
# site, siteobs, pedon, phorizon
.map <- list(site = list(siteiid = '.pID'), 
             #longstddecimaldegrees = 'x', latstddecimaldegrees = 'y'),
             siteobs = list(siteobsiid = '.pID'),
             pedon = list(peiid = '.pID'),
             phorizon = list(phiid = 'hzID', hzname = 'name',
                             hzdept = 'top', hzdepb = 'bottom',
                             phfield = 'pH', 
                             bounddistinct = 'distinctness',
                             boundtopo = 'topography'),
             phtexmod = list(texmod = 'texmod'),
             phtexture = list(texcl = 'texcl'))
tables <- remapColumns(x, .map)

### child tables that are "flattened" to site/horizon need to be denormalized
# phcolor
.map <- list(phcolor = list(phiidref =    c(dry = 'hzID', moist = 'hzID'),
                            colorhue =    c(dry = "dry_hue", moist = "moist_hue"),
                            colorvalue =  c(dry = "dry_value", moist = "moist_value"),
                            colorchroma = c(dry = "dry_chroma", moist = "moist_chroma")))
.keys <- list(phiidref = list(phcoloriid = 'colormoistst'))
tables$phcolor <- denormalizeColumns(aqp::horizons(x), .map, .keys)

# phtexture (for "RV" values -- 1:1 with phorizon)
tables$phtexture$phiidref <- tables$phorizon$phiid
tables$phtexture$phtextureiid <- 1:nrow(tables$phtexture)
tables$phtexmod$phtextureiidref <- tables$phtexmod$phtextureiid
tables$phtexmod$phtexmodiid <- 1:nrow(tables$phtexmod)

create_PedonPC_NASIS_import(tables, filename = "TEST.txt", overwrite = TRUE)
