library(tidyverse)
library(lubridate)
library(feather)
library(highcharter)

#### FILES and VARS ####
this_qtr_filenames <- 'f21q3'
last_qtr_filenames <- 'f21q2'
app_data_dir <- paste0('./data/dash/',this_qtr_filenames,'/')
current_q <- "2021_q3" 
report_date <- "2021/03/01"
uncat <- read_csv('./data/in/uncat_items.csv')

divisions <- read_csv('./data/in/divisions.csv') %>%
  mutate(center = ifelse(is.na(center),'SASB',center),
         center_color = case_when(center=='SASB' ~ '#fc8d62', #orange
                                  center=='LPA' ~ '#66c2a5', #grn
                                  center=='SCH' ~ '#8da0cb', #purple
                                  center=='SIBL' ~ '#e78ac3', #pink
                                  TRUE ~ as.character(center)),
         division_name = name) %>%
  select(-name)

ami_items <- read_feather(paste0("./data/in/",this_qtr_filenames,"_all_ami_items.feather"))
item_divs <- read_feather(paste0("./data/in/",this_qtr_filenames,"_all_item_divisions.feather"))

#### FUNCTIONS ####
get_mm_init <- function(scorefile, nodivfile, minmanddf_file) {
  minmand_init <- read_feather(scorefile)
  
  if (!'code' %in% names(minmand_init)) {
    minmand_init <- minmand_init %>% left_join(item_divs, by = c("mms_id" = "id"))
  }
  
  if (!is.null(nodivfile)) {
    nadiv <- minmand_init %>% filter(is.na(division_id)) %>% select(mms_id:captures, code, location)
    unkdiv <- minmand_init %>% filter(code %in% c('unk','null')) %>% select(mms_id:captures, code, location) %>% bind_rows(nadiv)
    write_csv(unkdiv, nodivfile)
  }
  
  minmand <- minmand_init %>%
    left_join(select(divisions, -id), by="code") %>%
    filter(!code %in% c('NO_DIV','EXTERNAL','EPE/CSW','CHILDCTR','PBL','RRS','unk','null'), !is.na(code)) %>% 
    mutate(center = ifelse(is.na(center),'SASB',center),
           ami = ifelse(mms_id %in% ami_items$id, "AMI", "Not AMI"))
  saveRDS(minmand, file = minmanddf_file)
  return(minmand)
}

get_plot_dfs <- function(minmanddf_file,uncat,current_q,report_date,app_data_dir,this_qtr_filenames,last_qtr_filenames,cat_only=FALSE) {
  suffix <- ifelse(cat_only == FALSE, '', '_cat')
  
  minmand <- if(cat_only == FALSE) {
    readRDS(file = minmanddf_file)
  } else {
    readRDS(file = minmanddf_file) %>%
      mutate(fy_q = current_q,
             catalogued = if_else(mms_id %in% uncat$mms_id & identifier == 0, FALSE, TRUE)) %>%
      filter(catalogued == TRUE)
  }

  # print(head(minmand))

  mm <- select(minmand, code,mms_id,total_min_mand,center,captures,ami) %>% 
    group_by(code,ami) %>% 
    summarise(items = n(), captures = sum(captures))
  saveRDS(mm, file = paste0(app_data_dir,this_qtr_filenames,'_mm',suffix,'.rds'))
  
  mm_prop <- if(cat_only == FALSE) {
    calc_prop(minmand, current_q)
  } else {
    calc_prop(minmand, current_q) %>% mutate(catalogued = TRUE)
  } 
  
  mm_prop_all <- readRDS(paste0('./data/dash/',last_qtr_filenames,'/',last_qtr_filenames,'_mm_prop',suffix,'.rds')) %>%
    bind_rows(mm_prop) %>%
    saveRDS(file = paste0(app_data_dir,this_qtr_filenames,'_mm_prop',suffix,'.rds'))
  
  scored_cols <- c('title', 'typeOfResource', 'genre', 'date', 'identifier', 'location', 'total_min_mand')
  mm_select <- get_selected_cols(minmand, scored_cols)
  count_sum <- calc_count_sum(mm_select, report_date)
  if (cat_only == FALSE) {
    qlab <- gsub(' \\d{2}(\\d{2})',paste0('_\\1'), count_sum$quarter_lab)[[1]]
    baseline_count_sum <- readRDS('./data/in/count_sum_baseline_all.rds')
    both_count_sums <- count_sum %>%
      ungroup() %>%
      mutate(element = fct_reorder(element, ele_order)) %>%
      select(element, ele_order, score, perc, quarter_lab) %>%
      pivot_wider(names_from = c(score, quarter_lab), values_from = perc, values_fill = list(perc = 0), names_prefix = "score_") %>%
      rename_all(~ gsub(' \\d{2}(\\d{2})',paste0('_\\1'), .)) %>%
      arrange(element) %>% 
      select(element,ele_order,ends_with(qlab)) %>%
      left_join(baseline_count_sum, by="element")
    write_csv(both_count_sums, paste0(app_data_dir,this_qtr_filenames,'_count_sum.csv'))
    
  } else {
    count_sum_baseline_cat <- readRDS('./data/in/count_sum_baseline_cat.rds')
    both_count_sums <- count_sum_baseline_cat %>% bind_rows(count_sum) %>%
      ungroup() %>%
      mutate(element = fct_reorder(element, ele_order)) %>%
      select(element, ele_order, score, perc, quarter_lab) %>%
      pivot_wider(names_from = c(score, quarter_lab), values_from = perc, values_fill = list(perc = 0), names_prefix = "score_") %>%
      rename_all(~ gsub(' \\d{2}(\\d{2})',paste0('_\\1'), .)) %>%
      arrange(element)
    write_csv(both_count_sums, paste0(app_data_dir,this_qtr_filenames,'_count_sum_cat.csv'))
  }
  
  elements <- c('title', 'typeOfResource', 'genre', 'date', 'identifier', 'location')
  combine_ebd_ext_app('./data/in/ebd_bsln',mm_select,report_date,elements,app_data_dir,this_qtr_filenames,cat_only=cat_only)

}

combine_ebd_ext_app <- function(baseline_ebd_path,mm_select,report_date,elements,app_data_dir,this_qtr_filenames,cat_only=FALSE){
  in_suffix <- ifelse(cat_only == FALSE, '', '_cat')
  out_suffix <- gen_out_suffix(cat_only,baseline_ebd_path)
  baseline_ebd <- readRDS(paste0(baseline_ebd_path,in_suffix,'.rds'))
  ebd <- get_element_by_division(mm_select, report_date, elements) %>% bind_rows(baseline_ebd)
  write_csv(ebd, paste0(app_data_dir,this_qtr_filenames,'_element_by_div',out_suffix,'.csv'))
}

which_minmand <- function(minmanddf_file,cat_ids,cat_only) {
  if(cat_only == FALSE) {
    readRDS(file = minmanddf_file)
  } else {
    readRDS(file = minmanddf_file) %>% filter(mms_id %in% cat_ids$mms_id)
  }
}

get_ext_plot_df <- function(minmanddf_file,cat_ids,elements,report_date,app_data_dir,this_qtr_filenames,baseline_ebd_path,cat_only=FALSE) {
  minmand <- which_minmand(minmanddf_file,cat_ids,cat_only)
  mm_select <- get_selected_cols(minmand, elements)
  combine_ebd_ext_app(baseline_ebd_path,mm_select,report_date,elements,app_data_dir,this_qtr_filenames,cat_only=cat_only)
}

get_ext_vb_dfs <- function(minmanddf_file,cat_ids,baseline_minmand_file,report_date,vb_elements,app_data_dir,this_qtr_filenames,cat_only=FALSE) {
  minmand <- which_minmand(minmanddf_file,cat_ids,cat_only)
  
  minmand_both <- combine_minmand_dfs(baseline_minmand_file,minmand)
  
  out_suffix <- gen_out_suffix(cat_only,minmanddf_file)
  
  # vb_elements <- c('genre','date','location','identifier')
  
  write_vb_dfs(minmand_both, vb_elements, app_data_dir, this_qtr_filenames, out_suffix, ext=TRUE)
}

gen_out_suffix <- function(cat_only, filepath) {
  case_when(cat_only == FALSE & str_detect(filepath,'ext') == TRUE ~ '_ext', 
            cat_only == TRUE & str_detect(filepath,'ext') == TRUE ~ '_ext_cat',
            cat_only == FALSE & str_detect(filepath,'app') == TRUE ~ '_ifapp', 
            cat_only == TRUE & str_detect(filepath,'app') == TRUE ~ '_ifapp_cat',
            cat_only == FALSE ~ '',
            cat_only == TRUE ~ '_cat')
}

combine_minmand_dfs <- function(baseline_minmand_file,minmand) {
  readRDS(file = baseline_minmand_file) %>% 
    mutate(code = NA,
           fy = "baseline", 
           coll_id = as.numeric(coll_id)) %>% 
    bind_rows(minmand %>% mutate(fy="current"))
}

get_vb_dfs <- function(minmanddf_file,baseline_minmand_file,app_data_dir,this_qtr_filenames,cat_only=FALSE) {
  suffix <- ifelse(cat_only == FALSE, '', '_cat')
  
  minmand <- if(cat_only == FALSE) {
    readRDS(file = minmanddf_file)
  } else {
    readRDS(file = minmanddf_file) %>%
      mutate(fy_q = current_q,
             catalogued = if_else(mms_id %in% uncat$mms_id & identifier == 0, FALSE, TRUE)) %>%
      filter(catalogued == TRUE)
  }
  
  minmand_both <- combine_minmand_dfs(baseline_minmand_file,minmand)
  
  vb_elements <- c('genre','date','location','identifier')
  write_vb_dfs(minmand_both, vb_elements, app_data_dir, this_qtr_filenames, suffix)
}

write_vb_dfs <- function(minmand_both, vb_elements, app_data_dir, this_qtr_filenames, suffix, ext=FALSE) {
  for (element in vb_elements) {
    if (ext == FALSE){ score_up <- get_score_up_vb(minmand_both, element) }
    else { score_up <- get_score_up_vb_ext(minmand_both, element) }
    print(paste0(element,suffix,' up:'))
    print(sum(score_up$n))
    efile <- ifelse(element == 'identifier', 'id', element) 
    saveRDS(score_up, file = paste0(app_data_dir,efile,'_up_',this_qtr_filenames,suffix,'.rds'))
  }
}

calc_prop <- function(minmand, fy_quarter) {
  minmand %>%
    select(mms_id, center, code, total_min_mand) %>%
    group_by(center, code) %>%
    summarize(n_recs = n(),
              n_above = sum(total_min_mand > 5),
              n_below = sum(total_min_mand <= 5),
              p_above = n_above / n_recs,
              p_below = n_below / n_recs) %>% 
    ungroup() %>%
    group_by(center) %>%
    arrange(desc(p_above), .by_group = TRUE) %>%
    mutate(fy_q = fy_quarter) %>%
    gather(prop_type, values, p_above:p_below)
}

get_selected_cols <- function(minmand, scored_elements) {
  minmand %>% select(mms_id, coll_id, code, center, c(!!!scored_elements))
}

get_element_by_division <- function(mm_select, report_time, elements) {
  mm_select %>%
    mutate(report_time = as.Date(report_time),
           quarter_lab = case_when(month(report_time) == 3 ~ paste('Mar',as.character(year(report_time))),
                                   month(report_time) == 6 ~ paste('Jun',as.character(year(report_time))),
                                   month(report_time) == 9 ~ paste('Sep',as.character(year(report_time))),
                                   month(report_time) == 12 ~ paste('Dec',as.character(year(report_time))))) %>%
    pivot_longer(c(!!!elements), names_to = "element", values_to = "score") %>%
    filter(!is.na(score)) %>%
    mutate(score = ifelse(!score %in% c(0,0.50,1.00),0.50,score)) %>%
    group_by(quarter_lab, code, element, score) %>%
    summarize(n = n()) %>%
    mutate(perc = round((n / sum(n))*100, digits = 2)) %>% 
    ungroup() %>%
    mutate(ele_order = case_when(element == 'title' ~ 1,
                                 element == 'typeOfResource' ~ 2,
                                 element == 'identifier' ~ 3,
                                 element == 'tms_identifiers' ~ 3.1,
                                 element == 'archives_identifiers' ~ 3.2,
                                 element == 'genre' ~ 4,
                                 element == 'date' ~ 5,
                                 element == 'location' ~ 6,
                                 element == 'language' ~ 1,
                                 element == 'name' ~ 2,
                                 element == 'form' ~ 3,
                                 element == 'note' ~ 4,
                                 element == 'tableOfContents' ~ 5,
                                 element == 'description' ~ 6,
                                 element == 'subject' ~ 7),
           element = fct_reorder(element, ele_order))
}

calc_count_sum <- function(minmand_select, report_time) {
  minmand_select %>%
    gather(element, score, title:location) %>%
    mutate(score = ifelse(!score %in% c(0,0.50,1.00),0.50,score)) %>%
    group_by(element, score) %>%
    summarize(n = n()) %>%
    mutate(perc = round((n / sum(n))*100, digits = 2),
           report_time = as.Date(report_time)) %>%
    mutate(ele_order = case_when(element == 'title' ~ 1,
                                 element == 'typeOfResource' ~ 2,
                                 element == 'identifier' ~ 3,
                                 element == 'genre' ~ 4,
                                 element == 'date' ~ 5,
                                 element == 'location' ~ 6),
           score_order = case_when(score == 0 ~ '0',
                                   score == 0.50 ~ '0.50',
                                   score == 0.75 ~ '0.75',
                                   score == 1 ~ '1.00'),
           score_order = factor(score_order, levels=c('1.00','0.75','0.50','0')),
           quarter_lab = case_when(month(report_time) == 3 ~ paste('Mar',as.character(year(report_time))),
                                   month(report_time) == 6 ~ paste('Jun',as.character(year(report_time))),
                                   month(report_time) == 9 ~ paste('Sep',as.character(year(report_time))),
                                   month(report_time) == 12 ~ paste('Dec',as.character(year(report_time))))) 
}

get_score_down <- function(minmand_both, element) {
  minmand_both %>%
    select(mms_id, !!element, fy) %>%
    pivot_wider(names_from = "fy", values_from = !!element) %>%
    mutate(remediation = current-baseline) %>%
    left_join(item_divs, by = c("mms_id" = "id")) %>%
    # left_join(select(divisions, id, code), by = c("division_id" = "id")) %>%
    filter(remediation < 0) %>% 
    mutate(element = element)
}

get_score_up_vb <- function(minmand_both, element) {
  minmand_both %>%
    select(mms_id, !!element, fy) %>%
    pivot_wider(names_from = "fy", values_from = !!element) %>%
    mutate(remediation = current-baseline) %>%
    left_join(item_divs, by = c("mms_id" = "id")) %>%
    filter(remediation > 0) %>% 
    count(code)
}

get_score_up_vb_ext <- function(minmand_both, element) {
  minmand_both %>%
    select(mms_id, !!element, fy) %>%
    pivot_wider(names_from = "fy", values_from = !!element) %>%
    mutate(current = ifelse(!is.na(baseline) & is.na(current), 1, current),
           baseline = ifelse(is.na(baseline) & !is.na(current), 1, baseline),
           remediation = current-baseline) %>%
    left_join(item_divs, by = c("mms_id" = "id")) %>%
    filter(remediation > 0) %>% 
    count(code)
}
  
  