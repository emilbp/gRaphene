## ---- message=FALSE------------------------------------------------------
library(tidyverse)
library(gRaphene)

cf_folder <- system.file("extdata/graphene_curve_fit_export", package = "gRaphene")
data_cf <- gr_cf_read(cf_folder)

## ------------------------------------------------------------------------
data_cf_filtered <- data_cf %>% filter(`G int` > 50) %>% select(-starts_with('SiO2'))

knitr::kable(
  gr_cf_summary(data_cf_filtered), 
  digits = c(0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 2, 2)
)

## ---- fig.show='hold'----------------------------------------------------
data_cf_small <- data_cf %>% select(x, y, `G int`) %>% rename(value = `G int`)
gr_cf_plot(df = data_cf_small, name = 'G int')
gr_cf_plot(df = data_cf_small, name = 'G int', scalebar = 2, palette = "Greys")

## ---- fig.width = 7, fig.height=8----------------------------------------
data_cf %>%
  mutate(condition = `G int` > 50) %>% 
  mutate(`2D FWHM` = ifelse(condition, `2D FWHM`, NA)) %>% 
  mutate(`D FWHM` = ifelse(condition, `D FWHM`, NA)) %>%  
  mutate(`G FWHM` = ifelse(condition, `G FWHM`, NA)) %>%  
  mutate(`D/G-ratio` = ifelse(condition, `D/G-ratio`, NA)) %>%
  mutate(`2D/G-ratio` = ifelse(condition, `2D/G-ratio`, NA)) %>% 
  gr_cf_plot_all()

