
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
specs_dir <- "specs/"

## ---- end-of-init

# About the Data ----
## ---- data-overview

# Load Data
dataset <- read.csv(paste0(data_dir,"cites_2001.csv"))
for (yy in 2002:2015) {
  dataset <- rbind(dataset, read.csv(paste0(data_dir,"cites_",yy,".csv")))
}

# Add Legends
dataset <- dataset %>% 
           left_join(read.csv(paste0(specs_dir,"cites_purpose.csv")), by="Purpose") %>%
           select(-Purpose) %>%
           rename(Purpose = Explanation) %>%
           left_join(read.csv(paste0(specs_dir,"cites_source.csv")), by="Source") %>%
           select(-Source) %>%
           rename(Source = Explanation)

cols_summary <- data_overview(dataset)
## ---- end-of-data-overview

# Pre-Processing ----
## ---- pp-restrict-animals

pre_clean <- nrow(dataset)

dataset <- dataset %>%
           filter(Class != "")

post_clean <- nrow(dataset)

## ---- end-of-pp-restrict-animals

## ---- pp-iucn-data

# Create an account with IUCN and download the csv from http://www.iucnredlist.org/search/saved?id=90695
# Move the csv into the data folder and rename it as iucn_list.csv

iucn_dataset <- read.csv(paste0(data_dir,"iucn_list.csv"), fileEncoding="latin1")

## ---- end-of-pp-iucn-data

## ---- pp-restrict-endanger

iucn_dataset <- iucn_dataset %>% 
                select(Order,
                       Family,
                       Genus,
                       Species,
                       CommonName = Common.names..Eng.,
                       IUCNStatus = Red.List.status)
iucn_dataset$IUCNStatus <- factor(iucn_dataset$IUCNStatus, 
                                  levels=c("EW","EX","VU","EN","CR"),
                                  ordered = TRUE)

# Different trades might have different level of granularities
# Some taxonomies only exist until Order, some until Family, and
# some until Genus. For such circumstances, we will assume the 
# worst case scenario, i.e. choose the most endangered status
# among species in the same family
species_iucn_list <- iucn_dataset %>%
                     mutate(Taxon = paste(Genus, Species)) %>%
                     select(Taxon, CommonName, IUCNStatus)
group_iucn <- function (granular) {
  tmp_iucn_list <- iucn_dataset
  tmp_iucn_list["Taxon"] <- lapply(tmp_iucn_list[granular],
                                   function (s) { paste0(toupper(substr(s,1,1)),tolower(substr(s,2,nchar(s))), " spp.")})
  tmp_iucn_list %>%
    group_by(Taxon) %>%
    summarise(CommonName = "",
              IUCNStatus = max(IUCNStatus))
}
genus_iucn_list <- group_iucn("Genus")
family_iucn_list <- group_iucn("Family")
order_iucn_list <- group_iucn("Order")
iucn_list <- rbind(species_iucn_list, 
                   genus_iucn_list, 
                   family_iucn_list, 
                   order_iucn_list)

pre_clean <- nrow(dataset)

dataset <- dataset %>%
           inner_join(iucn_list, by="Taxon")

post_clean <- nrow(dataset)

## ---- end-of-pp-restrict-endanger


# options(scipen = 999)
# red_list_data <- read.csv(paste0(data_dir,"iucn_list.csv"), fileEncoding="latin1")
# l <- c("Class","Order","Family","Genus","Species")
# 
# for (i in l) {
#   red_list_data[i] <- lapply(red_list_data[i],
#                                    function (x) {
#                                      paste0(toupper(substr(x,1,1)), tolower(substr(x,2,nchar(x))))
#                                    })
# }
# red_list_data <- red_list_data %>% mutate(Taxon = paste(Genus,tolower(Species)))
# ranking <- red_list_data %>% 
#            group_by(Taxon) %>% 
#             mutate(r = rank(desc(Year.assessed), ties.method = 'first')) %>% filter(r == 1)
# 
# data <- read.csv(paste0(data_dir,"2015.csv")) %>%
#         filter(Class != "" & Purpose != 'S') %>%
#         mutate(Qty = ifelse(is.na(Importer.reported.quantity),Exporter.reported.quantity,
#                             ifelse(is.na(Exporter.reported.quantity),Importer.reported.quantity,
#                                    ifelse(Exporter.reported.quantity > Importer.reported.quantity,
#                                           Exporter.reported.quantity,
#                                           Importer.reported.quantity)))) %>%
#         left_join(ranking %>% select(Taxon, Red.List.status), by=c("Taxon"))
# summary <- data  %>%
#             group_by(Term,Unit) %>%
#             summarise(Total = sum(Qty),
#                       Min = min(Qty),
#                       Max = max(Qty),
#                       Median = median(Qty),
#                       Mean = mean(Qty),
#                       SDev = sd(Qty),
#                       Species = n_distinct(Taxon),
#                       Class = n_distinct(Class),
#                       Order = n_distinct(Order),
#                       Family = n_distinct(Family),
#                       Genus = n_distinct(Genus))
# 
# converted <- data %>% left_join(summary %>% select(Term, Unit, Median), by=c("Term","Unit")) %>% mutate(LiveUnits = Qty / Median)
# cc <- converted %>% group_by(Taxon, Red.List.status) %>% summarise(LiveUnits = sum(LiveUnits))
# 
# cc_only_threatened <- cc %>% filter(Red.List.status %in% c('EX','EW','RE','CR','EN','VU'))
