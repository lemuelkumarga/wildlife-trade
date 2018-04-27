
# Initialization ----
## ---- init

# Load default settings for R Markdown -- see file for more details
source("shared/defaults.R")
# Load some helper functions
source("shared/helper.R")

options(stringsAsFactors = FALSE)
packages <- c("dplyr","ggplot2","tidyr","pander","DiagrammeR")
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

dataset <- dataset %>%
           filter(Class != "")

## ---- end-of-pp-restrict-animals

## ---- pp-restrict-endanger

# Create an account with IUCN and download the csv from http://www.iucnredlist.org/search/saved?id=90695
# Move the csv into the data folder and rename it as iucn_list.csv

iucn_dataset <- read.csv(paste0(data_dir,"iucn_list.csv"), fileEncoding="latin1")

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

dataset <- dataset %>%
           inner_join(iucn_list, by="Taxon")

## ---- end-of-pp-restrict-endanger

## ---- pp-terms-overview

# Take the majority between input and output
dataset <- dataset %>%
            mutate(Qty = ifelse(is.na(Importer.reported.quantity),Exporter.reported.quantity,
                         ifelse(is.na(Exporter.reported.quantity),Importer.reported.quantity,
                         ifelse(Exporter.reported.quantity > Importer.reported.quantity,
                                Exporter.reported.quantity,
                                Importer.reported.quantity))))

## ---- end-of-pp-terms-overview

## ---- pp-to-si

# A dictionary for converting scientific units to SI
# First item correspond to the SI unit
# and second the conversion factor
units_to_si <- list(
  "cm" = c("m", 0.01),
  "g" = c("kg",0.001),
  "pairs" = c("",2),
  "mg" = c("kg",1e-6),
  "l" = c("m3",0.001),
  "ml" = c("m3",1e-6),
  "ft2" = c("m2",0.092903),
  "cm2" = c("m2",0.0001),
  "cm3" = c("m3",1e-6),
  "microgrammes" = c("kg",1e-9)
)

dataset <- dataset %>% 
           mutate(ConvertedUnit = Unit,
                  ConvertedQty = Qty)

for (i in which(dataset$Unit %in% names(units_to_si))) {
  qty <- dataset[i,"Qty"]
  unit <- dataset[i,"Unit"]
  dataset[i,"ConvertedUnit"] <- units_to_si[[unit]][1]
  dataset[i,"ConvertedQty"] <- as.double(units_to_si[[unit]][2]) * qty
}
## ---- end-of-pp-to-si

## ---- pp-term-unit-summary
term_unit_counts <- dataset %>%
                    mutate(Unit = ConvertedUnit) %>%
                    group_by(Term,Unit) %>%
                    summarise(Records = n(),
                              Quantity = sum(ConvertedQty))

## ---- end-of-pp-term-unit-summary

## ---- pp-term-unit-target

target_unit <- term_unit_counts %>%
               ungroup() %>%
               group_by(Term) %>%
               mutate(r = rank(desc(Records), ties.method = 'first')) %>%
               summarise(
                 NumberOfUnits = n(),
                 TargetUnit = max(ifelse(r == 1, Unit, NA), na.rm=TRUE),
                 Records = max(ifelse(r == 1,Records,NA), na.rm=TRUE)
               )


## ---- end-of-pp-term-unit-target

## ---- pp-term-unit-final

get_median_dictionary <- function(data) {
  
  get_medians <- function (data, granularity) {
    output <- data
    if (granularity == "Kingdom") {
      output["Grouping"] <- "Animalia"
    } else {
      output["Grouping"] <- output[granularity]
    }
    output <- output %>% 
      filter(Grouping != '') %>%
      mutate(Unit = ConvertedUnit) %>%
      group_by(Grouping, Term, Unit) %>%
      summarise(
        Records = n(),
        Median = median(ConvertedQty)
      )
    return(output)
  }
  
  # Initalize variables
  taxonomy <- data %>% 
              mutate(Kingdom = "Animalia") %>%
              group_by(Kingdom,Class,Order,Family,Genus,Taxon) %>% 
              summarise() %>% ungroup()
  must_have_units <- data %>% 
                     group_by(Taxon, Term) %>% 
                     summarise() %>% 
                     left_join(target_unit %>% select(Term, TargetUnit), by="Term")
  median_dict <- get_medians(data, "Taxon") %>%
                 full_join(must_have_units, by=c("Grouping"="Taxon","Term"="Term","Unit"="TargetUnit")) %>%
                 mutate(Records = ifelse(is.na(Records),0,Records))
  
  for (granular in c("Genus","Family","Order","Class","Kingdom")) {
    
    # Get the medians for a particular granularity
    reference <- get_medians(data, granular) %>%
      rename(ProposedRecord = Records,
             ProposedMedian = Median)
    
    # Link the reference to the current dictionary
    linkname <- taxonomy %>%
      select_at(vars("Taxon",granular)) %>%
      rename_at(vars(granular), function(x) {"Link"})
    
    # Update all rows for which Records < 10
    median_dict <- median_dict %>%
      left_join(linkname, by=c("Grouping"="Taxon")) %>%
      left_join(reference, by=c("Link"="Grouping", "Term"="Term","Unit"="Unit")) %>%
      mutate(
        Median = ifelse((granular == 'Kingdom' & is.na(Median)) | 
                          (granular != 'Kingdom' & Records < 10 & !is.na(ProposedRecord)),
                        ProposedMedian, Median),
        Records = ifelse((granular == 'Kingdom' & Records == 0) |
                           (granular != 'Kingdom' & Records < 10 & !is.na(ProposedRecord)),
                         ProposedRecord, Records)
       
      ) %>%
      select(-Link, -ProposedRecord, -ProposedMedian)
  }
  return(median_dict)
}

median_dict <- get_median_dictionary(dataset)

dataset <- dataset %>%
            left_join(target_unit %>% select(Term, TargetUnit), by="Term") %>%
            left_join(median_dict %>% select(Grouping, Term, Unit, NonTargetMedian = Median),
                      by = c("Taxon"="Grouping",
                             "Term"="Term",
                             "ConvertedUnit"="Unit")) %>%
            left_join(median_dict %>% select(Grouping, Term, Unit, TargetMedian = Median),
                      by = c("Taxon"="Grouping",
                             "Term"="Term",
                             "TargetUnit"="Unit")) %>%
            mutate(
              ConvertedUnit = TargetUnit,
              ConvertedQty = ConvertedQty / NonTargetMedian * TargetMedian
            ) %>%
            select(-TargetUnit, -NonTargetMedian, -TargetMedian)

## ---- end-of-pp-term-unit-final

get_terms_summary <- function(dataset) {
  dataset %>%
    group_by(Term,ConvertedUnit) %>%
    summarise(Rows = n(),
              TotalTrades = sum(Qty),
              MedianTrades = median(Qty),
              UniqueClasses = n_distinct(Class),
              UniqueOrders = n_distinct(Order),
              UniqueFamily = n_distinct(Family),
              UniqueGenuses = n_distinct(Genus),
              UniqueSpecies = n_distinct(Taxon))
}

terms_summary <- get_terms_summary(dataset)


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
