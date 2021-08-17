1. Download latest proj-data from https://download.osgeo.org/proj/proj-data-1.7.tar.gz

 2. Extract data to a known path. A good option and the default for PROJ on Windows would be `$LOCALAPPDATA/proj/`. Another option which is the default for the sf package on Windows is the R library instance of the installation of sf ./proj directory `file.path(find.package("sf"), "proj")`. On Mac/Linux the default are the proj share directories in `/usr/local/share/proj` or `~/.local/share/proj`.

3. Restart your R session, reload {sf}

4. Append the path to your proj-data from step 2 to your search paths. You may want to add this to your .Rprofile.

```r
sf::sf_proj_search_paths(c(sf::sf_proj_search_paths(), "C:/Users/Andrew.G.Brown/AppData/Local/proj/")
```

If you re-install sf your existing data will be replaced with the package bundle. So, if you use your R library instance as the data location you will need to put your proj data back in place after each install. If you use your local application data / "share" folder then the files can persist between installs.

5. Compare the results of running the `?best_proj_pipeline` before and after setting the search path to point at your complete data package.
