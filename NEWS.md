# NASIStools 0.0.0.9006
* Added `get_NASIS_metadata()`: a function for accessing NASIS metadata with domain and choice-level granularity
* Added `lookupTexture()`/`lookupTextureModifier()`: helper functions for handling texture classes using NASIS metadata
* Added `create_PedonPC_NASIS_import()`: a function based on a generalization of Jay Skovlin's USFS NRM Pedon Database migration efforts. 
  - This tool generates a flat file with the NASIS table schema for Pedon object given a list of input tables. This template is based on imports of PedonPC data into NASIS. 
* Added `remapColumns()` and `denormalizeColumns()`: two functions useful for converting between schemas when using SoilProfileCollection or data.frame objects as an intermediate

# NASIStools 0.0.0.9005

* Added `get_correlation_info()` tool for getting legend, mapunit, datamapunit and component information for correlated and additional mapunits, given a related ID.
* Added a `NEWS.md` file to track changes to the package.

# NASIStools 0.0.0.9004

* Updates to `create_import_template()` tool for NASIS import XLSX file creation

# NASIStools 0.0.0.9003

* Add `ecositeid_to_name()` helper function for converting `ecositeid` to `ecositenm`

# NASIStools 0.0.0.9002

* Generic NASIS import template creator `create_import_template()`
* ESD Ecosites and ESD Notes templates via `create_ESD_ecosites_import()`, `create_note_from_ESD_ecosites()`, `create_ESD_notes_import()`

# NASIStools 0.0.0.9001

* Code converters 
* NASIS / Web Soil Survey SSURGO Export tools
* Interpretations "reasons" tools
