
#####################################################################
#####        Rscript untuk generate dataviz analisis DEA        #####
#####################################################################

#--> Clean env.
rm(list=ls())

#--> Load packages
library(tidyverse)
library(tidyr)
library(dplyr)
library(rio)
library(ggplot2)
library(scales)
library(patchwork)
library(sf)
library(ggspatial)
library(grid)



#--> Import Data
agr <- rio::import("Data/SUSENAS_SKI_AGR.dta")
id_shp <- sf::st_read("C:/Users/ASUS/OneDrive/DATASET BUAT OLAH-OLAH/idn_adm_bps_20200401_shp/idn_admbnda_adm1_bps_20200401.shp")

id_shp <- id_shp %>% mutate(kodeprov = substr(ADM1_PCODE,3,nchar(ADM1_PCODE))) %>% dplyr::select(., c(kodeprov))

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

## region
agr <- agr %>% mutate(island = case_when(
  kodeprov %in% c(10:30) ~ "Sumatera", kodeprov %in% c(31:40) ~ "Jawa",
  kodeprov %in% c(51:60) ~ "Bali-Nusra", kodeprov %in% c(61:70) ~ "Kalimantan",
  kodeprov %in% c(71:80) ~ "Sulawesi", kodeprov %in% c(81:90) ~ "Maluku",
  kodeprov %in% c(91:99) ~ "Papua")) %>% rename(kab = kodewil)


# 1) distribusi penduduk antar kabupaten kota
agr %>%
  group_by(source, kab) %>% 
  summarise(tot_pop = sum(pop, na.rm = TRUE), .groups = "drop") %>% 
  ggplot(aes(x = log(tot_pop), fill = source, group = source)) +
  geom_density(alpha = 0.3, adjust = 1) +
  labs(
    x = "Log(Total Populasi per Kode Wilayah)",
    y = "Density",
    fill = "Sumber Data"
  ) +
  theme_minimal()

ggsave("Output/pop-kernel-dens-kabkota.png",get_last_plot())

# 2) distribusi menurut kelompok umur
dat_dual <- agr %>% mutate(pop = pop/1e3) %>%
  group_by(source, usia) %>%
  summarise(
    mean_pop = mean(pop, na.rm = TRUE),
    sd_pop   = sd(pop, na.rm = TRUE),
    n        = sum(!is.na(pop)),
    .groups = "drop"
  ) %>% pivot_wider(names_from  = source,
                    values_from = c(mean_pop, sd_pop, n)
  ) %>% mutate(
    # rasio
    ratio_AB = mean_pop_SUSENAS / mean_pop_SKI,
    se_ratio = ratio_AB * sqrt((sd_pop_SUSENAS^2 / (n_SUSENAS * mean_pop_SUSENAS^2)) +
                                 (sd_pop_SKI^2      / (n_SKI      * mean_pop_SKI^2))),
    ratio_l = ratio_AB - 1.96 * se_ratio,
    ratio_u = ratio_AB + 1.96 * se_ratio,
    # rata-rata populasi antar source (untuk bar)
    mean_pop_avg = rowMeans(cbind(mean_pop_SUSENAS, mean_pop_SKI), na.rm = TRUE))

scale_factor <- max(dat_dual$mean_pop_avg, na.rm = TRUE) / 1.5


ggplot(dat_dual, aes(x = usia)) +
  # --- LHS: BAR ---
  geom_col(aes(y = mean_pop_avg),fill = "#4DBBD5",
           width = 0.8,alpha = 0.9) +
  # --- RHS: CI band ---
  geom_ribbon(aes(ymin = ratio_l * scale_factor,ymax = ratio_u * scale_factor),
    fill = "#E64B35",alpha = 0.2) +
  # --- RHS: LINE ---
  geom_line(aes(y = ratio_AB * scale_factor, group = 1),color = "#E64B35",linewidth = 1.2) +
  geom_point(aes(y = ratio_AB * scale_factor),
             color = "#E64B35",size = 2) +
  # --- ratio = 1 reference ---
  geom_hline(
    yintercept = 1 * scale_factor,
    linetype = "dashed",
    linewidth = 1,
    color = "black"
  ) + scale_y_continuous(
    name = "Rata-rata Jumlah Penduduk (SUSENAS+SKI)/2 (ribu orang)",
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Rasio Penduduk (SUSENAS / SKI)")) + 
  coord_cartesian(ylim = c(min(dat_dual$mean_pop_avg) * 0.9,scale_factor * 1.5)) + 
  labs(x = "Kelompok Usia") + theme_minimal()

ggsave("Output/pop-distrasio-by-age.png",get_last_plot())

# 3) distribusi prevalensi perokok berdasarkan provinsi
agr %>%
  group_by(source, kab) %>% 
  summarise(tot_pop = sum(pop, na.rm = TRUE),
            tot_rokok = sum(rokok_all, na.rm = T), .groups = "drop") %>% 
  ggplot(aes(x = tot_rokok/tot_pop, fill = source, group = source)) +
  geom_density(alpha = 0.3, adjust = 1) +
  labs(
    x = "Perokok tembakau/total populasi",
    y = "Density",
    fill = "Sumber Data"
  ) +
  theme_minimal()

ggsave("Output/smoke-kernel-dens-kabkota.png",get_last_plot())


# 4) distribusi prevalensi perokok berdasarkan usia
dat_ratio <- agr %>%
  group_by(source, usia) %>%
  summarise(tot_pop = sum(pop, na.rm = TRUE),
            rokok_all = sum(rokok_all, na.rm = TRUE),
            rokok_tembakau = sum(rokok_tembakau, na.rm = TRUE),
            rokok_elektrik = sum(rokok_elektrik, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(prev_all = rokok_all / tot_pop,
         prev_tembakau = rokok_tembakau / tot_pop,
         prev_elektrik = rokok_elektrik / tot_pop) %>%
  select(source, usia, tot_pop, starts_with("prev_")) %>% 
  pivot_longer(
    starts_with("prev_"),
    names_to = "jenis",
    values_to = "prev"
  ) %>% pivot_wider(names_from  = source,
                    values_from = c(prev, tot_pop),
                    names_sep   = "_") %>%
  mutate(ratio = prev_SUSENAS/prev_SKI,
         se_ratio = ratio * sqrt((1 - prev_SUSENAS) / (prev_SUSENAS * tot_pop_SUSENAS) +
                                   (1 - prev_SKI)     / (prev_SKI     * tot_pop_SKI)),
    ratio_l = ratio - 1.96 * se_ratio,
    ratio_u = ratio + 1.96 * se_ratio)


plot_ratio <- function(data, judul, ylim_use = NULL) {
  ggplot(data, aes(x = usia, y = ratio)) +
    geom_ribbon(
      aes(ymin = ratio_l, ymax = ratio_u),
      fill = "#1A7317",
      alpha = 0.25
    ) +
    geom_line(color = "#1A7317", linewidth = 1) +
    geom_point(color = "#1A7317", size = 2) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    coord_cartesian(ylim = ylim_use) +
    scale_y_continuous(name = "Rasio (SUSENAS / SKI)") +
    labs(x = "Usia", title = judul) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "italic"),
      axis.text.x = element_text(size = 8)
    )
}


p_all <- plot_ratio(dat_ratio %>% filter(jenis == "prev_all"),"Rasio %Merokok")
p_tembakau <- plot_ratio(dat_ratio %>% filter(jenis == "prev_tembakau"),"Rasio %Rokok Tembakau")
p_elektrik <- plot_ratio(dat_ratio %>% filter(jenis == "prev_elektrik"),"Rasio %Rokok Elektrik")

p_all | p_tembakau | p_elektrik

ggsave("Output/rasio-by-age.png",get_last_plot())
