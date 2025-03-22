install.packages("e1071") #Skewness and Kurtosis
install.packages("dynRB") #data
install.packages("tidyverse") #
install.packages("MASS") #Equations and data
install.packages("RColorBrewer") #colour 
install.packages("tibble") #tables and data wrangling
install.packages("ggplot2") #Graphs and other plots
install.packages("ggplotify") #Additional functions for graphs and plots
install.packages("rmarkdown") #Reproducible Research
install.packages("dynlm") #Dynamic linear models
install.packages("stats") #Statistical functions
install.packages("dplyr") #dynamic data wrangling
install.packages("tidyr") #cleaning up the data
install.packages("gtable") #Grob Tables for flexible formatting of tables
install.packages("glue") #cutting and pasting string data
install.packages("knitr") #Reports with the code included
install.packages("gt") #Flexible and colourful tables baby
install.packages("scales") #Provide methods for automatically determining breaks and labels for axes and legends.





df_heliobates <- fin2 %>%
  filter(str_detect(Species, 'Geospiza_heliobates')) %>% #filter the rows associated with the Species "Geospiza_heliobates"

df_prosthemelas <- fin2 %>% 
  filter(Species == "Geospiza_prosthemelas_prosthemelas")

df_parvula <- fin2 %>%
  filter(Species == "Geospiza_fuliginosa_parvula")

df_fortis <- fin2 %>%
  filter(Species == "Geospiza_fortis_fortis")

df_platyrhyncha <-fin2 %>%
  filter(Species == "Geospiza_fortis_platyrhyncha")


