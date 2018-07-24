# install and load packages
#if (!require ('pacman')) install.packages("pacman")
library(pacman)
p_load(tidyverse, drc, readxl, openxlsx)


# read data into r
a <- read_excel("data.xlsx")
a

# write a function to get absolute EC50
f.get_ec50 <- function(df){
  ec50.ll3 <- drm(df$relY ~ df$dose, fct = LL.3(fixed = c(NA, NA, NA),names = c("Slope", "Upper", "EC50")),
                  na.action = na.omit)
  ec50.abs <- data.frame(ED(ec50.ll3, respLev = c(50), type = "absolute", interval = "delta"), stringsAsFactors = FALSE)
  return(ec50.abs)
}

# use dplyr package to get ec50s in a data.frame
result <- a %>% 
  group_by(Fungicide, ID, Exp_rep) %>%  
  do(f.get_ec50(.))

# take an average of ec50 from Exp_rep 1 and 2
result1 <- result %>% 
  group_by(Fungicide, ID) %>% 
  summarise(EC50_abs = mean(Estimate, na.rm = TRUE))

write.xlsx(result1, "ec50_abs.xlsx")