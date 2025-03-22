

library(dynRB)
library(tidyverse)
library(tibble)
library(ggplot2)
library(ggplotify)
library(rmarkdown)
library(dplyr)
library(tidyr)
library(gtable)
library(glue)
library(knitr)

rm(list = ls()) #clear global environment


data("finch")   #fetch dataset
sex<-rep(c("F","M"),length(finch[,1])/2) #creating the new "sex" variable
fin2<-cbind(finch,sex) #adding the variable, as a new column, to the dataset
fin2 <- as.data.frame(fin2) %>% # Convert to data.frame for easier manipulation
  mutate(Species = case_when( 
    Species == "Geospiza_heliobates"    ~ "G. heliobates",      #Renaming species for readability
    
    Species == "Geospiza_prosthemelas_prosthemelas" ~ "G.p. prosthemelas",    #...
    
    Species == "Geospiza_fuliginosa_parvula"      ~ "G. f. parvula",    #...
    
    Species == "Geospiza_fortis_fortis"      ~ "G. f. fortis", #...........
    
    Species == "Geospiza_fortis_platyrhyncha"      ~ "G. f. platyrhyncha",#...
    
    
    TRUE ~ Species  # Keep other species  unchanged
  ))


finch_smry <- fin2 %>%
  group_by(Species) %>%
  dplyr::summarise(across(
    where(is.numeric),
    list(
      mean = ~ round(mean(., na.rm = TRUE), 3),
      sd = ~ round(sd(., na.rm = TRUE), 3),
      var = ~ round(var(., na.rm = TRUE), 3)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    cols = -Species,
    names_to = c("Trait", ".value"),
    names_sep = "_"
  ) %>%
  mutate(Mean_SD = paste(mean, "±", sd)) %>%
  dplyr::select(-mean, -sd) %>%
  pivot_wider(
    id_cols = Species,
    names_from = Trait,
    values_from = c(Mean_SD, var),
    names_glue = "{Trait}_{.value}"
  ) %>%
  finch_smry_ordered <- finch_smry %>%
  dplyr::select(
    Species,
    BodyL_Mean_SD,
    BodyL_var,
    WingL_Mean_SD,
    WingL_var,
    TailL_Mean_SD,
    TailL_var,
    BeakW_Mean_SD,
    BeakW_var,
    BeakH_Mean_SD,
    BeakH_var,
    LBeakL_Mean_SD,
    LBeakL_var,
    UBeakL_Mean_SD,
    UBeakL_var,
    `N.UBkL_Mean_SD`,
    `N.UBkL_var`
  ) %>%
gt() %>%
  cols_label(
    Species = "Species",
    BodyL_Mean_SD = "avg Body Length (± SD)",
    WingL_Mean_SD = "avg Wing Length (± SD)",
    TailL_Mean_SD = "avg Tail Length (± SD)",
    BeakW_Mean_SD = "avg Beak Width (± SD)",
    BeakH_Mean_SD = "avg Beak Height (± SD)",
    LBeakL_Mean_SD = "avg L.Beak Length (± SD)",
    UBeakL_Mean_SD = "avg U. Beak Length (± SD)",
    `N.UBkL_Mean_SD` = "N to U. Beak Length (± SD)",
    TarsusL_Mean_SD = "avg Tarsus Length (± sd)",
    BodyL_var = "BL var",
    WingL_var = "WL var",
    TailL_var = "TL var",
    BeakW_var = "BW var",
    BeakH_var = "BH var",
    LBeakL_var = "LBL var",
    UBeakL_var = "UBL var",
    N.UBkL_var = "NUBKL var",
    TarsusL_var= "TarsusL var"
  ) %>%
  data_color(
    columns = Species,
    rows = everything(),
    method = "factor",
    palette =  c(
      "G. f. fortis" = "purple",
      "G. f. parvula" = "green",
      "G. f. platyrhyncha" = "cyan",
      "G. heliobates" = "red",
      "G.p. prosthemelas" = "grey"),
    alpha = 0.8
  )
