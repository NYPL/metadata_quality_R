source('mq_functions.R')
#### MINMAND ####
scorefile <- paste0('./data/in/',this_qtr_filenames,'_scores.feather')
nodivfile <- paste0('./data/out/',this_qtr_filenames,'_no_div.csv')
minmanddf_file <- paste0('./data/min_mand_dfs/minmand_',this_qtr_filenames,'.rds')

minmand <- get_mm_init(scorefile, nodivfile, minmanddf_file)

#### minmand plot dfs ####
get_plot_dfs(minmanddf_file,uncat,current_q,report_date,app_data_dir,this_qtr_filenames,last_qtr_filenames,cat_only=FALSE)
get_plot_dfs(minmanddf_file,uncat,current_q,report_date,app_data_dir,this_qtr_filenames,last_qtr_filenames,cat_only=TRUE)

#### remdiation project valueboxes #### 
baseline_minmand_file <- "./data/min_mand_dfs/minmand_q3_2019_approved_tms.rds"
baseline_cat_minmand_file <- './data/min_mand_dfs/minmand_cat_baseline.rds'

get_vb_dfs(minmanddf_file,baseline_minmand_file,app_data_dir,this_qtr_filenames,cat_only=FALSE)
get_vb_dfs(minmanddf_file,baseline_cat_minmand_file,app_data_dir,this_qtr_filenames,cat_only=TRUE)

#### SCORED EXTENDED ####
cat_ids <- readRDS(minmanddf_file) %>%
  mutate(catalogued = if_else(mms_id %in% uncat$mms_id & identifier == 0, FALSE, TRUE)) %>%
  filter(catalogued == TRUE) %>%
  select(mms_id)

scoreextfile <- paste0('./data/in/',this_qtr_filenames,'_ext_scores.feather')
mmext_file <- paste0('./data/min_mand_dfs/extended/minmand_',this_qtr_filenames,'.rds')
minmand_ext <- get_mm_init(scoreextfile, NULL, mmext_file)

ext_elements <- c('title', 'typeOfResource', 'genre', 'date', 'tms_identifiers', 'archives_identifiers', 'location')
baseline_ebd_path <- './data/for_plots/ebd_bsln_mmext'
get_ext_plot_df(mmext_file,cat_ids,ext_elements,report_date,app_data_dir,this_qtr_filenames,baseline_ebd_path,cat_only=FALSE)
get_ext_plot_df(mmext_file,cat_ids,ext_elements,report_date,app_data_dir,this_qtr_filenames,baseline_ebd_path,cat_only=TRUE)

#### SCORED IF APPLICABLE ####
scoreifappfile <- paste0('./data/in/',this_qtr_filenames,'_ifapp_scores.feather')
mmifapp_file <- paste0('./data/min_mand_dfs/if_applicable/minmand_',this_qtr_filenames,'.rds')
minmand_ifapp <- get_mm_init(scoreifappfile, NULL, mmifapp_file)

scored_cols <- c('name', 'form', 'note', 'tableOfContents', 'description', 'subject','language')
baseline_ebd_path <- './data/for_plots/ebd_bsln_mmifapp'
get_ext_plot_df(mmifapp_file,cat_ids,scored_cols,report_date,app_data_dir,this_qtr_filenames,baseline_ebd_path,cat_only=FALSE)
get_ext_plot_df(mmifapp_file,cat_ids,scored_cols,report_date,app_data_dir,this_qtr_filenames,baseline_ebd_path,cat_only=TRUE)

#### EXT & IF APPLICABLE ValueBoxes ####
vb_elements <- c('typeOfResource', 'genre', 'date', 'tms_identifiers', 'archives_identifiers')
get_ext_vb_dfs(mmext_file,cat_ids,'./data/min_mand_dfs/extended/minmand_f19q3.rds',report_date,vb_elements,app_data_dir,this_qtr_filenames,cat_only=FALSE)
get_ext_vb_dfs(mmext_file,cat_ids,'./data/min_mand_dfs/extended/minmand_f19q3.rds',report_date,vb_elements,app_data_dir,this_qtr_filenames,cat_only=TRUE)

vb_elements <- c('language','name', 'form', 'note', 'tableOfContents', 'description', 'subject')
get_ext_vb_dfs(mmifapp_file,cat_ids,'./data/min_mand_dfs/if_applicable/minmand_f19q3.rds',report_date,vb_elements,app_data_dir,this_qtr_filenames,cat_only=FALSE)
get_ext_vb_dfs(mmifapp_file,cat_ids,'./data/min_mand_dfs/if_applicable/minmand_f19q3.rds',report_date,vb_elements,app_data_dir,this_qtr_filenames,cat_only=TRUE)
