
##########################################################
##### Rscript untuk menghitung CI                    #####
##### pada prevalensi perokok SKI dan SUSENAS        #####
##########################################################

#--> Clean env.
rm(list=ls())

#--> Load packages
library(tidyverse)
library(tidyr)
library(dplyr)
library(rio)
library(janitor)
library(tibble)
library(gt)
library(scales)
library(survey)

#--> Import Data
susenas <- rio::import("D:/dataset/BPS/Susenas/Susenas Maret 2023 - KOR/kor23_ind_1.dta")
ski     <- rio::import("D:/dataset/BPS/SKI 2023/CISDI/ski_INDIVIDU.dta")


susenas <- susenas %>% mutate(strata = paste0(R101, sprintf("%02d", R102), R105))


susenas <- susenas %>% 
  mutate(usia = R407) %>% filter(usia %in% c(10:75))

susenas <- susenas %>%
  mutate(kel_usia = cut(usia,
                        breaks = c(10, 14, 18, 25, 35, 45, 55, 65, Inf),
                        labels = c("10-14", "15-18", "19-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                        right  = FALSE))


susenas <- susenas %>% 
  mutate(rokok_tembakau = ifelse(R1207 %in% c(1,2),1,0),
         rokok_elektrik = ifelse(R1206 %in% c(1,2),1,0),
         rokok_all = ifelse(rokok_tembakau == 1| rokok_elektrik == 1,1,0))

ski <- ski %>% 
  mutate(usia = as.integer(as.character(B4K7THN))) %>% 
  filter(usia %in% c(10:75))


ski <- ski %>%
  mutate(kel_usia = cut(usia,
                        breaks = c(10, 14, 18, 25, 35, 45, 55, 65, Inf),
                        labels = c("10-14", "15-18", "19-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                        right  = FALSE))
ski <- ski %>% 
  mutate(rokok_tembakau = ifelse(G14A == 1 | G14B == 1 | G14C == 1 | G14E == 1,1,0),
         rokok_tembakau = ifelse(is.na(rokok_tembakau),0,rokok_tembakau),
         rokok_elektrik = ifelse(G14D == 1,1,0),
         rokok_elektrik = ifelse(is.na(rokok_elektrik),0,rokok_elektrik),
         rokok_all = ifelse(rokok_tembakau == 1| rokok_elektrik == 1,1,0))

options(survey.lonely.psu = "adjust")
design_susenas <- svydesign(
  id      = ~PSU,
  strata  = ~strata,
  weights = ~FWT,
  data    = susenas,
  nest    = TRUE)

design_ski <- svydesign(
  id      = ~PSU,
  strata  = ~STRATA,
  weights = ~w_final,
  data    = ski,
  nest    = FALSE)

susenas_tembakau <- svyby(
  formula = ~rokok_tembakau,
  by = ~kel_usia,
  design = design_susenas,
  FUN = svymean,
  vartype = "ci",
  level = 0.95)

ski_tembakau <- svyby(
  formula = ~rokok_tembakau,
  by = ~kel_usia,
  design = design_ski,
  FUN = svymean,
  vartype = "ci",
  level = 0.95)

susenas_tembakau$survei = "SUSENAS"
ski_tembakau$survei = "SKI"

tembakau <- rbind(susenas_tembakau,ski_tembakau)

susenas_elektrik <- svyby(formula = ~rokok_elektrik,
  by = ~kel_usia,design = design_susenas,
  FUN = svymean,vartype = "ci",level = 0.95)

ski_elektrik <- svyby(formula = ~rokok_elektrik,
  by = ~kel_usia,design = design_ski,
  FUN = svymean,vartype = "ci",level = 0.95)


susenas_elektrik$survei = "SUSENAS"
ski_elektrik$survei = "SKI"

elektrik <- rbind(susenas_elektrik,ski_elektrik)













library(tidyverse)
library(patchwork)

# --- Buat variabel usia_plot: integer untuk 10-64, "65+" untuk 65+ ---
susenas <- susenas %>%
  mutate(usia_plot = ifelse(usia >= 65, "65+", as.character(usia)))

ski <- ski %>%
  mutate(usia_plot = ifelse(usia >= 65, "65+", as.character(usia)))

# Rebuild design (karena data berubah)
design_susenas <- svydesign(
  id      = ~PSU,
  strata  = ~strata,
  weights = ~FWT,
  data    = susenas,
  nest    = TRUE)

design_ski <- svydesign(
  id      = ~PSU,
  strata  = ~STRATA,
  weights = ~w_final,
  data    = ski,
  nest    = FALSE)

# --- Fungsi hitung prevalensi per usia_plot ---
hitung_prevalensi <- function(var, design_s, design_k) {
  s <- svyby(
    formula = as.formula(paste0("~", var)),
    by      = ~usia_plot,
    design  = design_s,
    FUN     = svymean,
    vartype = "ci",
    level   = 0.95
  ) %>% mutate(survei = "SUSENAS")
  
  k <- svyby(
    formula = as.formula(paste0("~", var)),
    by      = ~usia_plot,
    design  = design_k,
    FUN     = svymean,
    vartype = "ci",
    level   = 0.95
  ) %>% mutate(survei = "SKI")
  
  rbind(s, k) %>%
    rename(prevalensi = all_of(var)) %>%
    # Urutkan: 10-64 numerik, lalu 65+ di akhir
    mutate(
      usia_num  = ifelse(usia_plot == "65+", 65L, as.integer(usia_plot)),
      usia_plot = fct_reorder(usia_plot, usia_num)
    )
}

df_tembakau <- hitung_prevalensi("rokok_tembakau", design_susenas, design_ski)
df_elektrik  <- hitung_prevalensi("rokok_elektrik", design_susenas, design_ski)

# --- Fungsi plot ---
buat_plot_ribbon <- function(data, judul, show_x = FALSE) {
  ggplot(data, aes(
    x     = usia_num,
    group = survei,
    color = survei,
    fill  = survei
  )) +
    geom_ribbon(
      aes(ymin = ci_l, ymax = ci_u),
      alpha = 0.15,
      color = NA
    ) +
    geom_line(
      aes(y = prevalensi),
      linewidth = 0.9
    ) +
    # Marker khusus di titik 65+ agar jelas ini kategori agregat
    geom_point(
      data      = filter(data, usia_plot == "65+"),
      aes(y     = prevalensi),
      size      = 3,
      shape     = 18   # diamond
    ) +
    scale_x_continuous(
      breaks = c(seq(10, 64, by = 5), 65),
      labels = c(seq(10, 64, by = 5), "65+"),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.1),
      expand = expansion(mult = c(0.05, 0.1))
    ) +
    scale_color_manual(values = c("SUSENAS" = "steelblue", "SKI" = "firebrick")) +
    scale_fill_manual(values  = c("SUSENAS" = "steelblue", "SKI" = "firebrick")) +
    labs(
      y     = "Prevalensi",
      x     = if (show_x) "Usia" else NULL,
      title = judul,
      color = NULL,
      fill  = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position  = if (show_x) "bottom" else "none",
      axis.text.x      = if (!show_x) element_blank() else element_text(size = 9),
      axis.ticks.x     = if (!show_x) element_blank() else element_line(),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 12)
    )
}

p_tembakau <- buat_plot_ribbon(df_tembakau, "Prevalensi Perokok Tembakau (C.I. 95%)")
p_elektrik  <- buat_plot_ribbon(df_elektrik, "Prevalensi Perokok Elektrik  (C.I. 95%)", show_x = TRUE)

p_tembakau / p_elektrik +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")