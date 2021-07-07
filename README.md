# metadata_quality_R

Scripts and functions for preparing quarterly metadata, [scored in python](https://github.com/NYPL/dc-tools/tree/master/metadataStats/metadataQuality), for visualization on the DCS dashboard and remediation sheet creation.

## Files

1. metadata_quality_R.Rproj
   - Click to open this R project file in Rstudio, which will open up your project workspace in the project's working directory. More on project files [here](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects).
2. renv directory and renv.lock
   - This file and directory contains information about all the installed R packages (and their specific versions) used by these scripts and in the dashboard. When opening up the project file for the first time on a new computer, run `renv::restore()` to install required packages (which may take a while) before proceeding. More on renv [here](https://rstudio.github.io/renv/articles/renv.html).
3. mq_functions.R 
   - This file loads the R packages and contains all the functions called by the other two scripts. At the start of a new quarter, update the variables containing date and quarter information before proceeding.
4. make_dash_data.R
   - This script takes in metadata quality data from our scoring script/notebook and creates files used for each plot and valueBox in the DCS dashboard metadata quality section. 
5. make_sheets.R
   - This script takes in metadata quality data from our make_dash_data.R script, compares metadata quality scores in the current and previous fiscal quarters, and creates spreadsheets by liaison division for remediation. 
   
## Data file structure

For these scripts to run successfully, create a `data/` sub-directory in the main metadata_quality_R directory. The data folder will need the following sub-folders:

```
-- data
    -- dash
        -- f21q3
        -- f21q2
    -- for_plots
    -- in
    -- min_mand_dfs
    -- out
```

The `min_mand_dfs`, `for_plots`, `in`, and `dash/f21q2` folders and files are available in the [MSU shared drive](https://drive.google.com/drive/folders/1m9_o_hce-swogY-SKdUzLKqgTVLZCE5e).

The `out` and `dash/f21q3` folders will be populated with files when the `make_dash_data.R` and `make_sheets.R` scripts are run.
