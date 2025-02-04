NEWS-KDHEtools
================

<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

    #> Last Update: 2025-02-03 15:55:03.813912

# KDHEtools v0.1.3.9002 (2025-02-03)

- update: Updated maximum input file size to 75mb.

# KDHEtools v0.1.3.9001 (2025-01-17)

- update: Updated taxa translator and attribute files.

# KDHEtools v0.1.3.0000 (2024-03-18)

- update: Increased file size limit to 50MB from 5MB

# KDHEtools v0.1.2.9066 (2023-10-25)

- fix: Minor updates throughout to text and linked files

# KDHEtools v0.1.2.9062 (2023-10-23)

- fix: In taxa translation routine in \*\_2taxamatch file flip “Changed”
  column was removed
- fix: Zip folder from IBI Calculator able to be opened using Extract
  All
- Update hyperlinks throughout documentation
- refactor: added new files to IBI Calculator output

# KDHEtools v0.1.2.9044 (2023-10-18)

- fix: In taxa translation routine in \*\_2taxamatch file flip “Changed”
  column

# KDHEtools v0.1.2.9043 (2023-09-06)

- fix: Change logo size from absolute to percentage,
  Blocktt/KDHEtools#11

# KDHEtools v0.1.2.9042 (2023-08-18)

- fix: MMI calc import change some columns to upper case
  - “SAMPLEID”, “LAT”, “LONG”, “TAXAID”, “N_TAXA”, “NONTARGET”,
    “AIRBREATHER”
  - Change Latitude and Longitude to LAT and LONG
- refactor: Add instructions to MMI calc to side bar
- refactor: Remove instructions tab from MMI calc

# KDHEtools v0.1.2.9041 (2023-08-18)

- refactor: MMI calc Shiny, change focus to import viewer after import

- refactor: Remove tab separated as accepted import file option, all
  imports

- docs: Add Calc MMI Instructions and link

- fix: Remove StationID as a required field for MMI calculator

- refactor: Changed links for KS MMI doc on background screen to load
  from Shiny app instead of opening GitHub folder

# KDHEtools v0.1.2.9040 (2023-08-17)

- fix: Fixed issue with separator when importing files.

- Reorganized app according to Jen Stamp’s suggestions

# KDHEtools v0.1.2.9031 (2023-08-15)

- fix: Added Exclude Taxa function from BioMonTools to index calculator
  function, Blocktt/KDHEtools#8

- Updated index calculator function to work properly from metric
  calculation to adjustment to scoring.

- Reorganized app according to Jen Stamp’s suggestions

# KDHEtools v0.1.2.9016 (2023-08-10)

- fix: Update changes in BioMonTools::taxa_translate,
  Blocktt/KDHEtools#4

- refactor: Update import functions in new sections to use all upper
  case names for default selections

  - Taxa Translator, Blocktt/KDHEtools#4

  - Predictors, Blocktt/KDHEtools#3

  - Merge Files, Blocktt/KDHEtools#1

# KDHEtools v0.1.2.9015 (2023-08-10)

- docs: Add packages as needed and pkg:: references, Blocktt/KDHEtools#7

  - shinythemes

  - lubridate

  - dplyr

- docs: Add Depends R \>= 3.5.0

- features: Add OTU selection to Taxa Translator

- features: Add Excluded Taxa check box to MMI calc, Blocktt/KDHEtools#8

  - Code not created to use it only the hook for it

# KDHEtools v0.1.2.9014 (2023-08-10)

- refactor: Shiny app set default import file separator in global

- docs: Remove unused packages from DESCRIPTION Suggests and global.R

  - Blocktt/KDHEtools#7

# KDHEtools v0.1.2.9013 (2023-08-10)

- docs: Remove unused packages from DESCRIPTION Imports and global.R
  - Blocktt/KDHEtools#7

# KDHEtools v0.1.2.9012 (2023-08-09)

- refactor: Remove errant browser debugging function

# KDHEtools v0.1.2.9011 (2023-08-09)

- refactor: Shiny app taxa translator and attribute assignment
  - Blocktt/KDHEtools#4

# KDHEtools v0.1.2.9010 (2023-08-08)

- refactor: Move predictors file name and details to global,
  Blocktt/KDHEtools#3

# KDHEtools v0.1.2.9009 (2023-08-08)

- refactor: Shiny app import add link to import table

  - Predictors, Blocktt/KDHEtools#3

  - Merge Files, Blocktt/KDHEtools#1

# KDHEtools v0.1.2.9008 (2023-08-08)

- docs: Referenced wrong issue, Blocktt/KDHEtools#3

# KDHEtools v0.1.2.9007 (2023-08-08)

- docs: Remove NAMESPACE_Ben

- feature: Add predictors code to server.R, Issue \#3

  - Blocktt/KDHEtools#3

# KDHEtools v0.1.2.9006 (2023-08-07)

- feature: Add taxa translator files to Shiny, Issue \#4
  - Blocktt/KDHEtools#4

# KDHEtools v0.1.2.9005 (2023-08-04)

- feature: Add file builder merge files to Shiny, Issue \#1
  - Blocktt/KDHEtools#1

# KDHEtools v0.1.2.9004 (2023-08-02)

- refactor: Rearrange packages in global.R to group those with no
  reference

# KDHEtools v0.1.2.9003 (2023-08-02)

- docs: Update DESCRIPTION

  - Change WY to KS

  - Update URL

  - Add BugReports

  - Move non referenced Imports Suggests

- refactor: Add comments to packages in global.R for those with no
  references

# KDHEtools v0.1.2.9002 (2023-08-01)

- docs: Ensure global.R and DESCRIPTION agree on packages

- refactor: Add package version to global and make variable for UI

- docs: Update DESCRIPTION title to KDHEtools

# KDHEtools v0.1.2.9001 (2023-08-01)

- docs: Update NEWS with information from commits on GitHub

  - Update NEWS version to match DESCRIPTION version number

- style: Updates to ReadMe

- docs: Add life cycle badge (Experimental) to ReadMe

- style: Move folder from package root to inst/ext

  - MetricDevelopment

- feat: Added data-raw folder

  - Added library creation helper script

- docs: Removed NAMESPACE as not created by roxygen2

  - Renamed as NAMESPACE_Ben

- style: Spacing

  - UI.R

  - Server.R

- docs: Add KHDEtools R file to document the package

# KDHEtools v0.0.1.9100 (2023-07-28)

- Updated RMD files according to Jen’s MS Word scripts

# KDHEtools v0.0.1.9040 (2023-07-28)

- Copied file builder user interface from leppott/BCGcalc to KDHEtools

- Added all necessary RMD files, images, and tables to recreate
  structure

- Currently “commented out” all functions so that only text and images
  display.

# KDHEtools v0.0.1.9032 (2023-07-28)

- Framework built for file builder

# KDHEtools v0.0.1.9031 (2023-07-28)

- Added new fields that are required to calculate BioMonTools metrics

# KDHEtools v0.0.1.9030 (2023-07-28)

- Added flags to predictor table

- Added new metrics to global.R to accomodate Jen’s flags

- Need to add required fields to input field requirements

# KDHEtools v0.0.1.9021 (2023-07-21)

- Updated background/instructions tabs

# KDHEtools v0.0.1.9020 (2023-07-21)

- Updated app background and instruction pages and accompanying tables

# KDHEtools v0.0.1.9011 (2023-07-19)

- Updates to randomForest model code.

# KDHEtools v0.0.1.9010 (2023-07-19)

- Added random forest code from Diane for KDHE models

# KDHEtools v0.0.1.9001 (2023-07-19)

- Small updates to server.R and global.R

# KDHEtools v0.0.1.9001 (2023-07-18)

- Minor updates to remove old info from WDEQtools

# KDHEtools readme (2023-07-18)

# KDHEtools readme (2023-07-18)

# KDHEtools 0.0.1.9000 (2023-07-18)

- Creation from copy of WY DEQ IBI calculator.

# KDHEtools Initial commit (2023-07-18)
