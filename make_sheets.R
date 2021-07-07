source('mq_functions.R')
coll_names <- read_feather(paste0('./data/in/',this_qtr_filenames,'_appd_collnames.feather'))

#### minimum mandatory ####
minmand_baseline <- readRDS(file = paste0("./data/min_mand_dfs/minmand_",last_qtr_filenames,".rds"))
minmand <- readRDS(file = paste0("./data/min_mand_dfs/minmand_",this_qtr_filenames,".rds"))
minmand_both <- minmand_baseline %>% 
  mutate(fy = "baseline", coll_id = as.numeric(coll_id), division_id = as.numeric(division_id)) %>% 
  bind_rows(minmand %>% mutate(fy="current", division_id = as.numeric(division_id)))

#### check missing ami ####
ami_thisQ <- filter(minmand, ami == "AMI")
ami_lastQ <- filter(readRDS(file = paste0('./data/min_mand_dfs/minmand_',last_qtr_filenames,'.rds')), ami == "AMI")

missing <- ami_lastQ %>% filter(!mms_id %in% ami_thisQ$mms_id) %>% 
  select(code,mms_id,total_min_mand,center,captures,ami) %>%
  mutate(missing_resource_type = ifelse(mms_id %in% tor$mms_id, TRUE, FALSE))

#### score down ####
genre_down <- get_score_down(minmand_both, "genre")
title_down <- get_score_down(minmand_both, "title")
date_down <- get_score_down(minmand_both, "date")
id_down <- get_score_down(minmand_both, "identifier")

tor <- minmand %>% 
  filter(typeOfResource != 1) %>% 
  select(mms_id:captures, code) %>% 
  mutate(element = 'typeOfResource') %>%
  left_join(coll_names, by="coll_id") %>%
  select(code,coll_id,coll_name,container_id,mms_id,item_name,captures,element)
mult_divs <- minmand %>%
  filter(location_4 == 0) %>%
  mutate(element = 'divisions issue') %>%
  left_join(coll_names, by="coll_id") %>%
  select(code,coll_id,coll_name,container_id,mms_id,item_name,captures,element)

#### score down ext ####
minmand_baseline <- readRDS(file = paste0("./data/min_mand_dfs/extended/minmand_",last_qtr_filenames,".rds"))
minmand <- readRDS(file = paste0("./data/min_mand_dfs/extended/minmand_",this_qtr_filenames,".rds"))
minmand_both <- minmand_baseline %>% 
  mutate(fy = "baseline", coll_id = as.numeric(coll_id), division_id = as.numeric(division_id)) %>% 
  bind_rows(minmand %>% mutate(fy="current", division_id = as.numeric(division_id)))

genre_down_ext <- minmand %>% filter(genre != 1) %>%
  mutate(element = 'genre auth/uri') %>%
  left_join(coll_names, by="coll_id") %>%
  select(code,coll_id,coll_name,container_id,mms_id,item_name,captures,element)

#### score down ifapp ####
# minmand_baseline <- readRDS(file = paste0("./data/min_mand_dfs/if_applicable/minmand_",last_qtr_filenames,".rds"))
ifapp <- readRDS(file = paste0("./data/min_mand_dfs/if_applicable/minmand_",this_qtr_filenames,".rds"))
# minmand_both <- minmand_baseline %>% 
#   mutate(fy = "baseline", coll_id = as.numeric(coll_id), division_id = as.numeric(division_id)) %>% 
#   bind_rows(minmand %>% mutate(fy="current", division_id = as.numeric(division_id)))

name_role <- ifapp %>% filter(name_5 != 1) %>%
  mutate(element = 'name role') %>%
  left_join(coll_names, by="coll_id") %>%
  select(code,coll_id,coll_name,container_id,mms_id,item_name,captures,element)
# unique(name_role$code)

#### WRITE ####
mm_info <- minmand %>% select(coll_id,container_id,mms_id,item_name,captures)
down <- genre_down %>% bind_rows(title_down) %>% bind_rows(date_down) %>% bind_rows(id_down) %>%
  left_join(mm_info, by="mms_id") %>%
  left_join(coll_names, by="coll_id") %>%
  select(code,coll_id,coll_name,container_id,mms_id,item_name,captures,element) %>%
  bind_rows(tor) %>% bind_rows(mult_divs) %>% bind_rows(genre_down_ext) %>% bind_rows(name_role) %>%
  mutate(item_url = paste0('https://metadata.nypl.org/items/',as.character(mms_id),'?section=desc_md'),
         coll_url = ifelse(is.na(coll_id), NA, paste0('https://metadata.nypl.org/collections/',as.character(coll_id),'?section=desc_md'))) %>%
  select(code,coll_id,coll_name,coll_url,container_id,mms_id,item_name,item_url,captures,element) %>%
  left_join(select(divisions, code, liaison), by="code") %>%
  mutate(liaison = ifelse(element %in% c('no repo','genre auth/uri'), 'KG', liaison))


# split df by liaison
split_df <- split(down, list(down$liaison))

# write out separate CSV for each liaison
mapply(function (data,liaison) 
  write_csv(data, 
            file = paste0("./data/out/",this_qtr_filenames,"_",liaison, ".csv"), 
            na = ""), 
  split_df, 
  names(split_df)
  )  

