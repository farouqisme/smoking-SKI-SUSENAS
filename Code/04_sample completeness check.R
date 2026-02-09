
##########################################################################
#####    Rscript untuk analisis kelengkapan data tingkat kab/kota    #####
##########################################################################

rm(list=ls())
#--> load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)
library(rio)
library(sf)
library(ggspatial)
library(grid)

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
  mutate(total_cat = total_cat - empty_both,
         susenas_perc = filled_susenas/total_cat,
         ski_perc = filled_ski/total_cat,
         sus_ski_rat = susenas_perc/ski_perc,
         sus_ski_dev = susenas_perc - ski_perc)

dat_for_map <- left_join(id_shp,comp_grid_agr,by=c("kodewil_susenaspre2022")) %>% filter(!is.na(total_cat))

#--> dataviz
# a) Rasio Kelengkapan demografi level pulau (deskriptif)
comp_grid_isl <- comp_grid %>% mutate(kodeprov = substr(kodewil_susenaspre2022,1,2),
         island = case_when(
           kodeprov %in% c(10:30) ~ "Sumatera", kodeprov %in% c(31:40) ~ "Jawa",
           kodeprov %in% c(51:60) ~ "Bali-Nusra", kodeprov %in% c(61:70) ~ "Kalimantan",
           kodeprov %in% c(71:80) ~ "Sulawesi", kodeprov %in% c(81:90) ~ "Maluku",
           kodeprov %in% c(91:99) ~ "Papua")) %>% group_by(island) %>%
  summarise(total_cat = n(),
            filled_susenas = sum(source_susenas == 1,na.rm = T),
            filled_ski = sum(source_ski == 2,na.rm = T),
            empty_both = sum(non_exist_both == 1,na.rm = T), .groups = "drop") %>%
  mutate(total_cat = total_cat - empty_both,
         susenas_perc = filled_susenas/total_cat,
         ski_perc = filled_ski/total_cat,
         sus_ski_rat = susenas_perc/ski_perc,
         sus_ski_dev = susenas_perc - ski_perc)

comp_grid_isl %>% 
  mutate(SUSENAS = scales::percent(susenas_perc,accuracy = 0.01),
         SKI = scales::percent(ski_perc,accuracy = 0.01),
         `SUSENAS/SKI` = scales::percent(sus_ski_rat,accuracy = 0.01),
         `SUSENAS-SKI` = scales::percent(sus_ski_dev,accuracy = 0.01)) %>%
  select(island,SUSENAS,SKI,`SUSENAS/SKI`,`SUSENAS-SKI`) %>% gt(rowname_col = "island",auto_align = FALSE)

# b) Jumlah kabupaten
sum(comp_grid_agr$sus_ski_dev > 0)
sum(comp_grid_agr$sus_ski_dev < 0)
sum(comp_grid_agr$sus_ski_dev == 0)



# 1) Rasio Kelengkapan demografi data SUSENAS 
dat_for_map %>%
  ggplot() + geom_sf(aes(fill = susenas_perc),color = NA,size  = 0.2) +
  annotation_north_arrow(location = "tr",which_north = "true",style = north_arrow_fancy_orienteering,
                         height = unit(1.2, "cm"),width  = unit(1.2, "cm")) +
  scale_fill_gradient2(
    low = "#BF0413",mid = "white",high = "#2E2C73",
    midpoint = 0.5,limits = c(0.0, 1),
    oob = scales::squish,na.value = "lightgray",name = "Rasio") +
  labs(title    = "Rasio Kelengkapan Demografi SUSENAS",
       subtitle = paste0("Min: ",percent(min(dat_for_map$susenas_perc),accuracy = 0.1),
                         " | Max: ",percent(max(dat_for_map$susenas_perc),accuracy = 0.1),
                         " | Mean: ",percent(mean(dat_for_map$susenas_perc),accuracy = 0.1),
                         " | Median: ",percent(median(dat_for_map$susenas_perc),accuracy = 0.1)),
    caption = paste0("Kelengkapan dihitung atas ketersedianya populasi pada beberapa kelompok sebagai berikut:\n",
                     "- Rentang usia 10-75 tahun\n",
                     "- Terdapat pada masing-masing klasifikasi wilayah Urban/Rural\n",
                     "- Tersedia pada jenis kelamin 1) Laki-laki dan 2) Perempuan")) +
  theme_minimal(base_size = 8) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),panel.grid = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title.position = "top",
        plot.title = element_text(face = "bold.italic", size = 10),
        plot.caption = element_text(face = "italic", hjust = 0))

ggsave("Output/rasio-kelengkapan-SUSENAS.png",get_last_plot())

# 2) Rasio Kelengkapan demografi data SKI
dat_for_map %>%
  ggplot() + geom_sf(aes(fill = ski_perc),color = NA,size  = 0.2) +
  annotation_north_arrow(location = "tr",which_north = "true",style = north_arrow_fancy_orienteering,
                         height = unit(1.2, "cm"),width  = unit(1.2, "cm")) +
  scale_fill_gradient2(
    low = "#BF0413",mid = "white",high = "#2E2C73",
    midpoint = 0.5,limits = c(0.0, 1),
    oob = scales::squish,na.value = "lightgray",name = "Rasio")+
  labs(title = "Rasio Kelengkapan Demografi SKI",
       subtitle = paste0("Min: ",percent(min(dat_for_map$ski_perc),accuracy = 0.1),
                         " | Max: ",percent(max(dat_for_map$ski_perc),accuracy = 0.1),
                         " | Mean: ",percent(mean(dat_for_map$ski_perc),accuracy = 0.1),
                         " | Median: ",percent(median(dat_for_map$ski_perc),accuracy = 0.1)),
       caption = paste0("Kelengkapan dihitung atas ketersedianya populasi pada beberapa kelompok sebagai berikut:\n",
                        "- Rentang usia 10-75 tahun\n",
                        "- Terdapat pada masing-masing klasifikasi wilayah Urban/Rural\n",
                        "- Tersedia pada jenis kelamin 1) Laki-laki dan 2) Perempuan")) +
  theme_minimal(base_size = 8) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),panel.grid = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title.position = "top",
        plot.title = element_text(face = "bold.italic", size = 10),
        plot.caption = element_text(face = "italic", hjust = 0))

ggsave("Output/rasio-kelengkapan-SKI.png",get_last_plot())


# 3) Selisih Rasio Kelengkapan demografi data SUSENAS-SKI 
dat_for_map %>%
  ggplot() + geom_sf(aes(fill = sus_ski_dev),color = NA,size  = 0.2) +
  annotation_north_arrow(location = "tr",which_north = "true",style = north_arrow_fancy_orienteering,
                         height = unit(1.2, "cm"),width  = unit(1.2, "cm")) +
  scale_fill_gradient2(
    low = "#BF0413",mid = "white",high = "#2E2C73",
    midpoint = 0,limits = c(-0.5, 0.5),
    oob = scales::squish,na.value = "lightgray",name = "Rasio") +
  labs(title    = "Selisih Rasio Kelengkapan Demografi (%SUSENAS - %SKI)",
       subtitle = paste0("Min: ",percent(min(dat_for_map$sus_ski_dev),accuracy = 0.1),
                         " | Max: ",percent(max(dat_for_map$sus_ski_dev),accuracy = 0.1),
                         " | Mean: ",percent(mean(dat_for_map$sus_ski_dev),accuracy = 0.1),
                         " | Median: ",percent(median(dat_for_map$sus_ski_dev),accuracy = 0.1)),
       caption = paste0("Kelengkapan dihitung atas ketersedianya populasi pada beberapa kelompok sebagai berikut:\n",
                        "- Rentang usia 10-75 tahun\n",
                        "- Terdapat pada masing-masing klasifikasi wilayah Urban/Rural\n",
                        "- Tersedia pada jenis kelamin 1) Laki-laki dan 2) Perempuan")) +
  theme_minimal(base_size = 8) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),panel.grid = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title.position = "top",
        plot.title = element_text(face = "bold.italic", size = 10),
        plot.caption = element_text(face = "italic", hjust = 0))

ggsave("Output/selisih-rasio-kelengkapan.png",get_last_plot())
