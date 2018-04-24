
# Initialization ----
## ---- init

# Load default settings for R Markdown -- see file for more details
source("shared/defaults.R")
# Load some helper functions
source("shared/helper.R")

options(stringsAsFactors = FALSE)
packages <- c("dplyr","ggplot2","tidyr","pander")
load_or_install.packages(packages)

data_dir <- "data/"
output_dir <- "output/"

## ---- end-of-init

options(scipen = 999)
data <- read.csv(paste0(data_dir,"2015.csv"))
summary <- data %>% 
          filter(Class != "") %>%
          mutate(Qty = ifelse(is.na(Importer.reported.quantity),Exporter.reported.quantity,
                              ifelse(is.na(Exporter.reported.quantity),Importer.reported.quantity,
                                     ifelse(Exporter.reported.quantity > Importer.reported.quantity,
                                            Exporter.reported.quantity,
                                            Importer.reported.quantity)))) %>%
          group_by(Term,Unit) %>%
          summarise(Total = sum(Qty),
                    Min = min(Qty),
                    Max = max(Qty),
                    Median = median(Qty),
                    Mean = mean(Qty),
                    SDev = sd(Qty),
                    Species = n_distinct(Taxon),
                    Class = n_distinct(Class),
                    Order = n_distinct(Order),
                    Family = n_distinct(Family),
                    Genus = n_distinct(Genus))
