
#####################################################################
#####        Rscript untuk generate dataviz analisis DEA        #####
#####################################################################

#--> Clean env.
rm(list=ls())

#--> Load packages
library(dplyr)
library(ggplot2)
library(ggspatial)
library(grid)
library(patchwork)
library(rio)
library(scales)
library(sf)
library(tidyr)
library(tidyverse)



#--> Import Data
agr <- rio::import("Data/SUSENAS_SKI_AGR.dta")
concord <- rio::import("Data/relasi kode kabupaten susenas ski.xlsx")
concord <- concord %>% mutate(kodewil_ski = as.character(kodewil_ski),
                              kodewil_susenaspre2022 = as.character(kodewil_susenaspre2022),
                              kodewil_susenas = as.character(kodewil_susenas))
id_shp <- sf::st_read("C:/Users/ASUS/OneDrive/DATASET BUAT OLAH-OLAH/idn_adm_bps_20200401_shp/idn_admbnda_adm2_bps_20200401.shp")
id_shp <- id_shp %>% mutate(kodewil = substr(ADM2_PCODE,3,nchar(ADM2_PCODE))) %>% dplyr::select(., c(kodewil))

#--> Labeling
## Age group
agr <- agr %>% 
  mutate(usia_group = case_when(usia <= 18 ~ "10-18",
                                usia %in% c(19:25) ~ "19-25", usia %in% c(26:30) ~ "26-30",
                                usia %in% c(31:35) ~ "31-35", usia %in% c(36:40) ~ "36-40",
                                usia %in% c(41:45) ~ "41-45", usia %in% c(46:50) ~ "46-50",
                                usia %in% c(51:55) ~ "51-55", usia %in% c(56:60) ~ "56-60",
                                usia %in% c(61:65) ~ "61-65", usia > 65 ~ ">66"))
## data source, urban/rural, sex
agr <- agr %>%
  mutate(source = factor(source,levels = c(1, 2),labels = c("SUSENAS", "SKI")),
         urban = factor(urban,levels = c(0, 1),labels = c("Rural", "Urban")),
         jenis_kelamin = factor(jenis_kelamin,levels = c(1,2),labels = c("Laki-laki","Perempuan")))

## matching kodekab
ski <- agr %>% filter(source == "SKI")
ski <- left_join(ski,concord,by=c("kodewil" = "kodewil_ski"))
ski <- ski %>% dplyr::select(., -c(kodewil,kodewil_susenas)) %>% rename(kodewil = kodewil_susenaspre2022)

susenas <- agr %>% filter(source == "SUSENAS")

agr_matched_code <- bind_rows(susenas,ski)

rio::export(agr_matched_code,"Data/merokok agregat susenas-ski.csv")

## region
agr <- agr_matched_code %>% mutate(island = case_when(
  kodeprov %in% c(10:30) ~ "Sumatera", kodeprov %in% c(31:40) ~ "Jawa",
  kodeprov %in% c(51:60) ~ "Bali-Nusra", kodeprov %in% c(61:70) ~ "Kalimantan",
  kodeprov %in% c(71:80) ~ "Sulawesi", kodeprov %in% c(81:90) ~ "Maluku",
  kodeprov %in% c(91:99) ~ "Papua")) 



make_ratio_map <- function(
    agr,
    id_shp,
    usia_group_filter = NULL,
    map_type = c("all", "tembakau", "elektrik"),
    map_title = "Peta Rasio Prevalensi",
    midpoint = 0,
    limits = c(-0.3,0.3)
) {
  
  
  map_type <- match.arg(map_type)
  
  ## 1. filter usia_group (opsional)
  if (!is.null(usia_group_filter)) {
    agr <- agr %>% filter(usia_group %in% usia_group_filter)
  }
  
  ## 2. hitung prevalensi & rasio
  map_prev <- agr %>%
    group_by(source, kodewil) %>%
    summarise(
      tot_pop        = sum(pop, na.rm = TRUE),
      tot_rokok      = sum(rokok_all, na.rm = TRUE),
      tot_tembakau   = sum(rokok_tembakau, na.rm = TRUE),
      tot_elektrik   = sum(rokok_elektrik, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      prev_all        = tot_rokok / tot_pop,
      prev_tembakau   = tot_tembakau / tot_pop,
      prev_elektrik   = tot_elektrik / tot_pop
    ) %>%
    pivot_wider(
      names_from  = source,
      values_from = c(prev_all, prev_tembakau, prev_elektrik)
    ) %>%
    group_by(kodewil) %>%
    summarise(across(everything(), ~ first(na.omit(.x))), .groups = "drop") %>%
    mutate(
      ratio_all       = prev_all_SUSENAS - prev_all_SKI,
      ratio_tembakau  = prev_tembakau_SUSENAS - prev_tembakau_SKI,
      ratio_elektrik  = prev_elektrik_SUSENAS - prev_elektrik_SKI
    )
  
  ## 3. pilih variabel kontinu sesuai map_type
  fill_var <- switch(
    map_type,
    all        = "ratio_all",
    tembakau  = "ratio_tembakau",
    elektrik  = "ratio_elektrik"
  )
  
  ## 4. gabung shapefile
  map_wshp <- id_shp %>% left_join(map_prev, by = "kodewil")
  
  ## 5. ringkasan statistik untuk subtitle
  fill_vals <- map_wshp[[fill_var]]
  subtitle_dyn <- paste0(
    "Min: ", percent(min(fill_vals, na.rm = TRUE),accuracy = 0.01),
    " | Max: ", percent(max(fill_vals, na.rm = TRUE),accuracy = 0.01),
    " | Mean: ", percent(mean(fill_vals, na.rm = TRUE),accuracy = 0.01),
    " | Median: ", percent(median(fill_vals, na.rm = TRUE),accuracy = 0.01))
  
  ## 6. plot kontinu
  ggplot() +
    geom_sf(
      data  = map_wshp,
      aes(fill = .data[[fill_var]]),
      color = NA,
      size  = 0.2
    ) +
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = north_arrow_fancy_orienteering,
      height = unit(1.2, "cm"),
      width  = unit(1.2, "cm")
    ) +
    scale_fill_gradient2(
      low = "#BF0413",
      mid = "white",
      high = "#2E2C73",
      midpoint = midpoint,
      limits = limits,
      oob = scales::squish,
      na.value = "lightgray",
      name = "Rasio"
    ) +
    labs(
      title    = map_title,
      subtitle = subtitle_dyn,
      caption  = paste0(
        "Interpretasi:\n",
        "- Selisih > 0  : prevalensi SUSENAS > SKI\n",
        "- Selisih = 0  : prevalensi setara\n",
        "- Selisih < 0  : prevalensi SUSENAS < SKI\n",
        "Sumber: SUSENAS & SKI (diolah)"
      )
    ) +
    theme_minimal(base_size = 8) +
    theme(
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      axis.title       = element_blank(),
      panel.grid       = element_blank(),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.title.position = "top",
      plot.title       = element_text(face = "bold.italic", size = 10),
      plot.subtitle    = element_text(size = 9, color = "gray40"),
      plot.caption     = element_text(face = "italic", hjust = 0)
    )
}


## 1. Selisih prevalensi merokok
make_ratio_map(
  agr        = agr,
  id_shp     = id_shp,
  map_type   = "all",
  limits     = c(-0.4,0.4),
  map_title  = "Peta Selisih Prevalensi Merokok (SUSENAS - SKI)")

ggsave("Output/map-merokok-all.png",get_last_plot())

## 1.1. Selisih prevalensi merokok usia 10-18
make_ratio_map(
  agr        = agr,
  id_shp     = id_shp,
  usia_group_filter = c("10-18"),
  map_type   = "all",
  limits     = c(-0.4,0.4),
  map_title  = "Peta Selisih Prevalensi Merokok Usia 10-18 (SUSENAS - SKI)")

ggsave("Output/map-merokok-10-18.png",get_last_plot())

## 1.2. Selisih prevalensi merokok usia 26-30
make_ratio_map(
  agr        = agr,
  id_shp     = id_shp,
  usia_group_filter = c("26-30"),
  map_type   = "all",
  limits     = c(-0.4,0.4),
  map_title  = "Peta Selisih Prevalensi Merokok Usia 26-30 (SUSENAS - SKI)")

ggsave("Output/map-merokok-26-30.png",get_last_plot())

## 1.3. Selisih prevalensi merokok usia 51-55
make_ratio_map(
  agr        = agr,
  id_shp     = id_shp,
  usia_group_filter = c("51-55"),
  map_type   = "all",
  limits     = c(-0.4,0.4),
  map_title  = "Peta Selisih Prevalensi Merokok Usia 51-55 (SUSENAS - SKI)")

ggsave("Output/map-merokok-51-55.png",get_last_plot())


## 2. Selisih prevalensi rokok tembakau
make_ratio_map(
  agr        = agr,
  id_shp     = id_shp,
  map_type   = "tembakau",
  limits     = c(-0.4,0.4),
  map_title  = "Peta Selisih Prevalensi Merokok Tembakau (SUSENAS - SKI)")

ggsave("Output/map-tembakau-all.png",get_last_plot())

## 2.1. Selisih prevalensi rokok tembakau usia 10-18
make_ratio_map(
  agr        = agr,
  id_shp     = id_shp,
  usia_group_filter = c("10-18"),
  map_type   = "tembakau",
  limits     = c(-0.4,0.4),
  map_title  = "Peta Selisih Prevalensi Merokok Tembakau Usia 10-18 (SUSENAS - SKI)")

ggsave("Output/map-tembakau-10-18.png",get_last_plot())

## 2.2. Selisih prevalensi rokok tembakau usia 26-30
make_ratio_map(
  agr        = agr,
  id_shp     = id_shp,
  usia_group_filter = c("26-30"),
  map_type   = "tembakau",
  limits     = c(-0.4,0.4),
  map_title  = "Peta Selisih Prevalensi Merokok Tembakau Usia 26-30 (SUSENAS - SKI)")

ggsave("Output/map-tembakau-26-30.png",get_last_plot())

## 2.3. Selisih prevalensi rokok tembakau usia 51-55
make_ratio_map(
  agr        = agr,
  id_shp     = id_shp,
  usia_group_filter = c("51-55"),
  map_type   = "tembakau",
  limits     = c(-0.4,0.4),
  map_title  = "Peta Selisih Prevalensi Merokok Tembakau Usia 51-55 (SUSENAS - SKI)")

ggsave("Output/map-tembakau-51-55.png",get_last_plot())

## 3. Selisih prevalensi rokok elektrik
make_ratio_map(
  agr        = agr,
  id_shp     = id_shp,
  map_type   = "elektrik",
  limits     = c(-0.1,0.1),
  map_title  = "Peta Selisih Prevalensi Merokok Elektrik (SUSENAS - SKI)")

ggsave("Output/map-elektrik-all.png",get_last_plot())

## 2.1. Selisih prevalensi merokok elektrik usia 10-18
make_ratio_map(
  agr        = agr,
  id_shp     = id_shp,
  usia_group_filter = c("10-18"),
  map_type   = "elektrik",
  limits     = c(-0.1,0.1),
  map_title  = "Peta Selisih Prevalensi Merokok Elektrik Usia 10-18 (SUSENAS - SKI)")

ggsave("Output/map-elektrik-10-18.png",get_last_plot())

## 2.2. Selisih prevalensi merokok elektrik usia 26-30
make_ratio_map(
  agr        = agr,
  id_shp     = id_shp,
  usia_group_filter = c("26-30"),
  map_type   = "elektrik",
  limits     = c(-0.1,0.1),
  map_title  = "Peta Selisih Prevalensi Merokok Elektrik Usia 26-30 (SUSENAS - SKI)")

ggsave("Output/map-elektrik-26-30.png",get_last_plot())

## 2.3. Selisih prevalensi merokok elektrik usia 51-55
make_ratio_map(
  agr        = agr,
  id_shp     = id_shp,
  usia_group_filter = c("51-55"),
  map_type   = "elektrik",
  limits     = c(-0.1,0.1),
  map_title  = "Peta Selisih Prevalensi Merokok Elektrik Usia 51-55 (SUSENAS - SKI)")

ggsave("Output/map-elektrik-51-55.png",get_last_plot())