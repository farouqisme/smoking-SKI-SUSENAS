
###########################################################################
#####        Rscript untuk generate dataviz untuk analisis DEA        #####
###########################################################################

#--> Clean env.
rm(list=ls())

#--> Load packages
library(tidyverse)
library(tidyr)
library(dplyr)
library(rio)
library(ggplot2)
library(scales)

#--> Import Data
agr <- rio::import("Data/SUSENAS_SKI_AGR.dta")

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
agr <- agr %>% mutate(prov = substr(kodewil,1,2), island = case_when(
  prov %in% c(10:30) ~ "Sumatera", prov %in% c(31:40) ~ "Jawa",
  prov %in% c(51:60) ~ "Bali-Nusra", prov %in% c(61:70) ~ "Kalimantan",
  prov %in% c(71:80) ~ "Sulawesi", prov %in% c(81:90) ~ "Maluku",
  prov %in% c(91:99) ~ "Papua")) %>% rename(kab = kodewil)


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


# 4) distribusi prevalensi perokok berdasarkan kabupaten kota
# 3) distribusi prevalensi perokok berdasarkan perokok