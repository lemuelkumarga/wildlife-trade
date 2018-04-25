
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
red_list_data <- read.csv(paste0(data_dir,"red_list.csv"), fileEncoding="latin1")
l <- c("Class","Order","Family","Genus","Species")

for (i in l) {
  red_list_data[i] <- lapply(red_list_data[i],
                                   function (x) {
                                     paste0(toupper(substr(x,1,1)), tolower(substr(x,2,nchar(x))))
                                   })
}
red_list_data <- red_list_data %>% mutate(Taxon = paste(Genus,tolower(Species)))
ranking <- red_list_data %>% 
           group_by(Taxon) %>% 
            mutate(r = rank(desc(Year.assessed), ties.method = 'first')) %>% filter(r == 1)

data <- read.csv(paste0(data_dir,"2015.csv")) %>%
        filter(Class != "" & Purpose != 'S') %>%
        mutate(Qty = ifelse(is.na(Importer.reported.quantity),Exporter.reported.quantity,
                            ifelse(is.na(Exporter.reported.quantity),Importer.reported.quantity,
                                   ifelse(Exporter.reported.quantity > Importer.reported.quantity,
                                          Exporter.reported.quantity,
                                          Importer.reported.quantity)))) %>%
        left_join(ranking %>% select(Taxon, Red.List.status), by=c("Taxon"))
summary <- data  %>%
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

converted <- data %>% left_join(summary %>% select(Term, Unit, Median), by=c("Term","Unit")) %>% mutate(LiveUnits = Qty / Median)
cc <- converted %>% group_by(Taxon, Red.List.status) %>% summarise(LiveUnits = sum(LiveUnits))

cc_only_threatened <- cc %>% filter(Red.List.status %in% c('EX','EW','RE','CR','EN','VU'))
