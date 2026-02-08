
##########################################################################
#####    Rscript untuk analisis kelengkapan data tingkat kab/kota    #####
##########################################################################

rm(list=ls())
#--> load packages
library(tidyverse)
library(dplyr)

#--> import data
raw_data <- rio::import("Data/SUSENAS_SKI_AGR.dta")
nmprov <- rio::import("Data/Nama Provinsi BPS (38 provinsi).xlsx") %>% mutate(kodeprov = as.character(kodeprov))
concord <- rio::import("Data/relasi kode kabupaten susenas ski.xlsx")
id_shp <- sf::st_read("C:/Users/ASUS/OneDrive/DATASET BUAT OLAH-OLAH/idn_adm_bps_20200401_shp/idn_admbnda_adm2_bps_20200401.shp")

concord <- concord %>% mutate(kodewil_susenaspre2022 = as.character(kodewil_susenaspre2022),
                              kodewil_ski = as.character(kodewil_ski),
                              kodewil_susenas = as.character(kodewil_susenas))

id_shp <- id_shp %>% mutate(kodewil_susenaspre2022 = substr(ADM2_PCODE,3,nchar(ADM2_PCODE))) %>% 
  dplyr::select(., c(kodewil_susenaspre2022,ADM2_EN))
  
susenas <- raw_data %>% filter(source == 1)
ski <- raw_data %>% filter(source == 2)

#--> checking data completeness
susenas <- left_join(susenas,concord,by=c("kodewil" = "kodewil_susenaspre2022"))
susenas <- susenas %>% dplyr::select(., -c(kodewil)) %>% rename(kodewil = kodewil_susenas)

ski <- left_join(ski,concord,by=c("kodewil" = "kodewil_ski"))
ski <- ski %>% dplyr::select(., -c(kodewil)) %>% rename(kodewil = kodewil_susenas)

comp_grid <- expand_grid(kodewil = concord$kodewil_susenas,  
                         urban = c(0, 1),jenis_kelamin = c(1, 2),usia = 10:75)
comp_grid <- comp_grid %>% mutate(kodeprov = substr(kodewil,1,2))
comp_grid <- left_join(comp_grid,nmprov,by=c("kodeprov"))
comp_grid <- left_join(comp_grid,concord %>% 
                         dplyr::select(.,c(kodewil_susenaspre2022,kodewil_susenas)),by=c("kodewil" = "kodewil_susenas"))

t_susenas <- susenas %>% rename(source_susenas = source) %>% 
  dplyr::select(., c(kodewil,urban,jenis_kelamin,usia,source_susenas)) 
t_ski <- ski  %>% rename(source_ski = source) %>%
  dplyr::select(., c(kodewil,urban,jenis_kelamin,usia,source_ski))

comp_grid <- left_join(comp_grid,t_susenas,by=c("kodewil","urban","jenis_kelamin","usia"))
comp_grid <- left_join(comp_grid,t_ski,by=c("kodewil","urban","jenis_kelamin","usia"))

comp_grid <- comp_grid %>% mutate(non_exist_both = ifelse(is.na(source_susenas) & is.na(source_ski),1,NA))

comp_grid_agr <- comp_grid %>% group_by(kodewil_susenaspre2022) %>% 
  summarise(total_cat = n(),
            filled_susenas = sum(source_susenas == 1,na.rm = T),
            filled_ski = sum(source_ski == 2,na.rm = T),
            empty_both = sum(non_exist_both == 1,na.rm = T), .groups = "drop")

comp_grid_agr <- comp_grid_agr %>% 
  mutate(susenas_perc = filled_susenas/total_cat,
         ski_perc = filled_ski/total_cat,
         empty_both_perc = empty_both/total_cat,
         sus_ski_rat = susenas_perc/ski_perc)

dat_for_map <- left_join(id_shp,comp_grid_agr,by=c("kodewil_susenaspre2022")) %>% filter(!is.na(total_cat))

#--> dataviz


