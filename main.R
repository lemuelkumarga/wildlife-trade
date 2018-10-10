
# Initialization ----
## ---- init
# Disable scientific notation
options(scipen=999)
# Load default settings for R Markdown -- see file for more details
source("shared/defaults.R")
# Load some helper functions
source("shared/helper.R")

options(stringsAsFactors = FALSE)
# To install streamgraph, use devtools::install_github("hrbrmstr/streamgraph")
# To install rgdal, you may need to follow these instructions on Mac
# https://stackoverflow.com/questions/34333624/trouble-installing-rgdal
# (Kudos to @Stophface)
packages <- c("dplyr","ggplot2","tidyr","pander","scales","DiagrammeR","data.table","parallel",
              "htmlwidgets","streamgraph","purrr","EnvStats", "waffle","sunburstR","rgdal",
              "leaflet","colorspace","toOrdinal","igraph","ggraph","grDevices")
load_or_install.packages(packages)

data_dir <- "data/"
specs_dir <- "specs/"

## ---- end-of-init

# About the Data ----
## ---- data-about

# Load Data
dataset <- cache("2001_2015_dataset",list(),function() {
  dataset <- read.csv(paste0(data_dir,"cites_2001.csv"))
  for (yy in 2002:2015) {
    dataset <- rbind(dataset, read.csv(paste0(data_dir,"cites_",yy,".csv")))
  }
  
  # Add Legends
  dataset %>% 
  left_join(read.csv(paste0(specs_dir,"cites_purpose.csv")), by="Purpose") %>%
  select(-Purpose) %>%
  rename(Purpose = Explanation) %>%
  left_join(read.csv(paste0(specs_dir,"cites_source.csv")), by="Source") %>%
  select(-Source) %>%
  rename(Source = Explanation)
})


cols_summary <- data_overview(dataset)
## ---- end-of-data-about

# Pre-Processing ----
## ---- pp-animals

dataset <- dataset %>%
  filter(Class != "")

## ---- end-of-pp-animals

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

dataset <- cache("si_converted_data", list(dataset=dataset), function(dataset) {
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
  
  rows_to_modify <- which(dataset$Unit %in% names(units_to_si))
  for (i in which(dataset$Unit %in% names(units_to_si))) {
    qty <- dataset[i,"Qty"]
    unit <- dataset[i,"Unit"]
    dataset[i,"Unit"] <- units_to_si[[unit]][1]
    dataset[i,"Qty"] <- as.double(units_to_si[[unit]][2]) * qty
  }
  
  dataset
})


## ---- end-of-pp-to-si

## ---- pp-term-unit-summary
term_unit_counts <- dataset %>%
  group_by(Term,Unit) %>%
  summarise(Records = n(),
            Quantity = sum(Qty))

## ---- end-of-pp-term-unit-summary

## ---- pp-term-unit-target

target_unit <- term_unit_counts %>%
  ungroup() %>%
  group_by(Term) %>%
  mutate(r = rank(desc(Records), ties.method = 'first')) %>%
  summarise(
    NumberOfUnits = n(),
    Unit = max(ifelse(r == 1, Unit, NA), na.rm=TRUE),
    Records = max(ifelse(r == 1,Records,NA), na.rm=TRUE)
  )

## ---- end-of-pp-term-unit-target

## ---- pp-term-unit-final

# Convert the term and units of the dataset to their desired targets
# @input data: the dataset
# @input target: the target term-unit pairs. If we are converting
# terms to standardized units, target should contain 2 columns: Term and Unit.
# If we are converting terms to 1 term, target should contain 1 column: Term.
convertViaMedian <- function(data, target) {
  
  convert_term <- length(colnames(target)) == 1
  # Add a Converted column to keep track which quantities have been converted and which has not
  if (!("Converted" %in% colnames(data))) {
    data["Converted"] <- FALSE
  }
  
  # Initalize variables
  taxonomy <- data %>% 
    mutate(Kingdom = "Animalia") %>%
    group_by(Kingdom,Class,Order,Family,Genus,Taxon) %>% 
    summarise() %>% ungroup()
  
  if (convert_term) {
    must_have_units <- data %>% 
                      group_by(Taxon) %>%
                      summarise()
    must_have_units["Term"] <- target[1,"Term"]
    must_have_units["Unit"] <- ""
  } else {
    must_have_units <- data %>% 
      group_by(Taxon, Term) %>% 
      summarise() %>% 
      left_join(target, by="Term")
  }
  
  
  # Function to find the medians for each granularity groupings (Species, Genus, etc)
  get_medians <- function (data, granularity) {
    output <- data
    if (granularity == "Kingdom") {
      output["Grouping"] <- "Animalia"
    } else {
      output["Grouping"] <- output[granularity]
    }
    output <- output %>% 
      filter(Grouping != '') %>%
      group_by(Grouping, Term, Unit) %>%
      summarise(
        Records = n(),
        Median = median(Qty)
      )
    return(output)
  }
  
  # Create the median dictionaries which contains a Taxon, Term and Unit pair, along with their
  # median trade quantities
  median_dict <- get_medians(data, "Taxon") %>%
    full_join(must_have_units, by=c("Grouping"="Taxon","Term"="Term","Unit"="Unit")) %>%
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
                          (granular != 'Kingdom' & Records < 10 & !is.na(ProposedMedian)),
                        ProposedMedian, Median),
        Records = ifelse((granular == 'Kingdom' & Records == 0) |
                           (granular != 'Kingdom' & Records < 10 & !is.na(ProposedRecord)),
                         ProposedRecord, Records)
        
      ) %>%
      select(-Link, -ProposedRecord, -ProposedMedian)
  }
  
  # Convert the term-unit from their original to the target using the median roll-up approach
  if (convert_term) {
    data["TargetTerm"] <- target[1,"Term"]
    data["TargetUnit"] <- ""
  } else {
    data <- data %>%
      left_join(target %>% select(Term, TargetUnit = Unit), by="Term") %>%   
      mutate(TargetTerm = Term)
  }
  
  data <- data %>%
    left_join(median_dict %>% select(Grouping, Term, Unit, NonTargetMedian = Median),
              by = c("Taxon"="Grouping",
                     "Term"="Term",
                     "Unit"="Unit")) %>%
    mutate(ScaledQty = Qty / NonTargetMedian)
  
  outlier_limits <- quantile(data$ScaledQty, c(0.05,0.95))
  
  data <- data %>%
          mutate(ScaledQty = ifelse(ScaledQty > outlier_limits[[2]],outlier_limits[[2]],
                               ifelse(ScaledQty < outlier_limits[[1]], outlier_limits[[1]],ScaledQty))) %>%
          left_join(median_dict %>% select(Grouping, Term, Unit, TargetMedian = Median),
                    by = c("Taxon"="Grouping",
                           "TargetTerm"="Term",
                           "TargetUnit"="Unit")) %>%
          mutate(
            Term = TargetTerm,
            Unit = TargetUnit,
            Qty = ifelse(NonTargetMedian == TargetMedian, Qty, ScaledQty * TargetMedian),
            Converted = NonTargetMedian != TargetMedian | Converted
          ) %>%
          select(-ScaledQty, -TargetTerm, -TargetUnit, -NonTargetMedian, -TargetMedian)
  
  return(data)
}

dataset <- cache("units_standardized_data", list(dataset=dataset), function(dataset) {
  convertViaMedian(dataset, target_unit %>% select(Term, Unit))
})

## ---- end-of-pp-term-unit-final

## ---- pp-term-well-defined-overview

well_defined_terms <- read.csv(paste0(specs_dir, "well_defined_terms.csv"))

## ---- end-of-pp-term-well-defined-overview

## ---- pp-term-well-defined-convert

dataset <- dataset %>%
  left_join(well_defined_terms, by="Term") %>%
  mutate(isWellDefined = !is.na(perAnimal),
         Term = ifelse(isWellDefined, "animal",Term),
         Unit = ifelse(isWellDefined, "", Unit),
         Qty = ifelse(isWellDefined, 1. / perAnimal, 1.) * Qty) %>%
  select(-perAnimal, -isWellDefined)

## ---- end-of-pp-term-well-defined-convert

## ---- pp-term-ambiguous-convert

dataset <- cache("standardized_data", list(dataset=dataset),function(dataset) {
  target <- data_frame(Term = "animal")
  convertViaMedian(dataset, target)
})

## ---- end-of-pp-term-ambiguous-convert

## ---- pp-taxon

dataset <- cache("complete_taxon_data", list(dataset=dataset),function(dataset) {

  # Condense dataset to speed up processing
  dataset <- dataset %>% mutate(FromMissingTaxon = FALSE)
  condenseData <- function(dataset) {
    dataset %>%
    group_by(Year, Taxon, Class, Order, Family, Genus, Importer, Exporter, Origin, Term, Unit, Purpose, Source, Converted, FromMissingTaxon) %>%
    summarise(Qty = sum(Qty)) %>%
    ungroup()
  }
  dataset <- condenseData(dataset)
    
  # Find all missing taxonomy records, sorted by the missing information degree
  missing_taxon <- dataset %>% 
                   filter((Taxon == Class | grepl("spp.",Taxon)) & !is.na(Exporter) & !is.na(Importer)) %>%
                   arrange(desc(Genus), desc(Family), desc(Order), desc(Class))
  
  # Special case for hybrids
  hybrid_taxons <- dataset %>%
                   filter(grepl("hybrid",Taxon))
  
  dataset_hybrid <- hybrid_taxons %>%
                    filter(Genus != "")
  
  missing_hybrid <- hybrid_taxons %>%
                    filter(Genus == "")
  
  # Convert missing taxonomy data to complete taxonomy
  # @input completed_taxon a dataset containing only complete taxonomy records
  # @input r the missing taxonomy record to insert
  # @output the new complete taxonomy records based on r
  convertMissingTaxon <- function(completed_taxon, r) {
    
    # Find all the completed taxonomies associated with the missing row
    referenced_taxon <- completed_taxon %>%
                        filter((r$Class == "" | r$Class == Class) &
                               (r$Order == "" | r$Order == Order) &
                               (r$Family == "" | r$Family == Family) &
                               (r$Genus == "" | r$Genus == Genus))
    
    # If there are no references, return an empty row
    if (nrow(referenced_taxon) == 0) { return(r %>% filter(1==2)) }
    
    # Create a taxonomy dictionary which returns the distribution
    # of complete taxonomies. The granularity will determine
    # if this distribution was from trade routes, exports of 
    # exporter, imports of importer, or the whole universe of
    # trades
    taxonDictionary <- function(granularity) {
      referenced_taxon %>%
        mutate(
          Importer = sapply(Importer, function (i) { ifelse(granularity %in% c("EXPORT","ALL"),r$Importer,i)}),
          Exporter = sapply(Exporter, function (e) { ifelse(granularity %in% c("IMPORT","ALL"),r$Exporter,e)})
        ) %>%
        filter(r$Exporter == Exporter & r$Importer == Importer) %>%
        group_by(Taxon, Class, Order, Family, Genus, Exporter, Importer) %>%
        summarise(Qty = sum(Qty)) %>%
        ungroup() %>%
        mutate(Ratio = Qty / sum(Qty)) %>%
        select(NewTaxon = Taxon, 
               NewClass = Class,
               NewOrder = Order,
               NewFamily = Family,
               NewGenus = Genus, 
               Ratio)
    }
    
    # Merge the old row with the dictionary and spread the
    # quantity with the percentages in the dictionary.
    # If the most granular dictionary does not have any
    # data, proceed to a less granular dictionary
    for (g in c("ROUTE","EXPORT","IMPORT","ALL")) {
      taxon_dict <- taxonDictionary(g)
      if (nrow(taxon_dict) == 0) { next; }
      new_rs <- merge(r, taxon_dict, all=TRUE) %>%
                  mutate(Taxon = NewTaxon,
                         Class = NewClass,
                         Order = NewOrder,
                         Family = NewFamily,
                         Genus = NewGenus,
                         Qty = Ratio * Qty,
                         FromMissingTaxon = TRUE) %>%
                  select(-NewTaxon, -NewClass, -NewOrder, -NewFamily, -NewGenus, -Ratio)
      return(new_rs)
      
    }
    
  }
  
  # Insert the missing taxonomies one by one
  insertMissingTaxon <- function(dataset, rows) {
    
    #Pack the dataset to ensure faster processing
    condensed_dataset <- dataset %>%
                          group_by(Taxon, Class, Order, Family, Genus, Exporter, Importer) %>%
                          summarise(Qty = sum(Qty)) %>%
                          ungroup()
    
    # Prepare cores for parallel processing
    n_cores <- detectCores() - 1
    cl <- makeCluster(n_cores, outfile="")
    clusterEvalQ(cl, { library("dplyr"); library("scales")})
    clusterExport(cl, c("condensed_dataset","rows"), environment())
    clusterExport(cl, c("convertMissingTaxon"), parent.env(environment()))
    
    new_rs <- parLapply(cl, 1:nrow(rows),function(i) {
                cat("|")
                missing_row <- rows[i,]
                convertMissingTaxon(condensed_dataset, missing_row)
              })
     
     stopCluster(cl)
    
     condenseData(rbind(dataset,rbindlist(new_rs, use.names=TRUE)))
  }
  
  dataset <- dataset %>%
             filter(!grepl("spp.|hybrid",Taxon) & Taxon != Class)
 
  # First insert taxonomies with missing species
  no_species <- missing_taxon %>% filter(Genus != "")
  dataset <- insertMissingTaxon(dataset, no_species)
  # Then insert taxonomies with missing genus
  no_genus <- missing_taxon %>% filter(Family != "" & Genus == "")
  dataset <- insertMissingTaxon(dataset, no_genus)
  # Then insert taxonomies with missing family
  no_family <- missing_taxon %>% filter(Order != "" & Family == "")
  dataset <- insertMissingTaxon(dataset, no_family)
  # Then insert taxonomies with missing order
  no_order <- missing_taxon %>% filter(Class != "" & Order == "")
  dataset <- insertMissingTaxon(dataset, no_order)
  
  # Finally, settle the hybrids dataset, which is a smaller subset
  dataset_hybrid <- insertMissingTaxon(dataset_hybrid, missing_hybrid)
  dataset <- rbind(dataset, dataset_hybrid)
  
  # Next get taxonomies
  # Condense the dataset
  dataset 
})

## ---- end-of-pp-taxon

## ---- pp-endanger

# Create an account with IUCN and download the csv from http://www.iucnredlist.org/search/saved?id=90695
# Move the csv into the data folder and rename it as iucn_list.csv

iucn_dataset <- read.csv(paste0(data_dir,"iucn_list.csv"), fileEncoding="latin1")

iucn_dataset <- iucn_dataset %>% 
  select(Order,
         Family,
         Genus,
         Species,
         CommonName = Common.names..Eng.,
         IUCNStatus = Red.List.status) %>%
  mutate(Taxon = paste(Genus, Species)) %>%
  select(Taxon, CommonName, IUCNStatus)

# Choose only the first english name out of the list
iucn_dataset[['CommonName']] <- sapply(iucn_dataset[['CommonName']], function(x) { strsplit(x,", ")[[1]][1] })

dataset <- dataset %>%
           inner_join(iucn_dataset, by="Taxon")

## ---- end-of-pp-endanger

# Exploration ----

## ---- exp-time

trades_by_time <- dataset %>%
                  filter(!(IUCNStatus %in% c("EW","EX"))) %>%
                  mutate(IUCNLabel = ifelse(IUCNStatus == "CR", "Critical",
                                            ifelse(IUCNStatus == "EN", "Endangered",
                                                   "Vulnerable"))) %>%
                  group_by(Year,IUCNLabel) %>%
                  summarise(total_trades = round(sum(Qty))) %>%
                  ungroup()

# Add Percentage Increment
trades_by_time <- trades_by_time %>%
                  left_join(trades_by_time %>% 
                              mutate(NextYear = Year + 1) %>%
                              select(NextYear, IUCNLabel, prev_trades = total_trades), 
                            by=c("Year" = "NextYear", "IUCNLabel" = "IUCNLabel")) %>%
                  mutate(pct_inc = (total_trades - prev_trades) / prev_trades)

trades_by_time$IUCNLabel <- factor(trades_by_time$IUCNLabel, 
                                    levels=c("Vulnerable","Endangered","Critical"),
                                    ordered = TRUE)

streamgraph_plot <- suppressWarnings(
                      streamgraph(trades_by_time, key="IUCNLabel", value="total_trades", date="Year", offset="zero",
                                                   width=700, height=280,left=30) %>%
                      sg_colors(axis_color = `@c`(ltxt), tooltip_color = `@c`(ltxt)) %>%
                      # Set ticks to once every two years
                      sg_axis_x(2) %>%
                      sg_axis_y(tick_format = "s") %>%
                      sg_fill_manual(c(`@c`(red), `@c`(red, 0.6), `@c`(red, 0.3)))
                      
                    )

# Add annotation for interesting points
for (iucn_lbl in c("Critical","Endangered","Vulnerable")) {

  if (iucn_lbl == "Critical") {
    iucn_filter <- c("Vulnerable","Endangered","Critical")
  } else if (iucn_lbl == "Endangered") {
    iucn_filter <- c("Vulnerable","Endangered")
  } else {
    iucn_filter <- c("Vulnerable")
  }
  
  x <- 2011:2015
  
  # Positioning of Labels
  x_str <- function (yy) { sprintf("%s-%s-01", yy - 1,
                                   ifelse(yy == 2011,"07",
                                   ifelse(yy == 2015, 
                                          ifelse(iucn_lbl == "Endangered","04","06"),"11"))) }
  
  y_shift <- ifelse(iucn_lbl %in% "Endangered", -225000, -280000)
  y <- function (yy) { (trades_by_time %>%
                          filter(IUCNLabel %in% iucn_filter & Year == yy) %>%
                          summarise(total_trades = sum(total_trades)))$total_trades + y_shift }
  
  # Labels for 2011 and 2012-2015
  label_color <- ifelse(iucn_lbl == "Vulnerable", `@c`(txt), `@c`(bg))
  label_abs <- function (yy) {
                  val <- (trades_by_time %>% 
                          filter(Year == yy & IUCNLabel == iucn_lbl))$total_trades
                  sprintf("%.2f mil",val / 1000000)
                }
  label_pct <- function(yy) { 
                  val <- (trades_by_time %>% 
                          filter(Year == yy & IUCNLabel == iucn_lbl))$pct_inc
                  sprintf("%s%.0f%%", ifelse(val > 0, "+", "-"),abs(round(val * 100)))
               }
  
  # Append Labels!
  pwalk(list(x),
        function(yy) {
          if (yy == 2011) { label <- label_abs(yy) } else { label <- label_pct(yy) }
          streamgraph_plot <<- streamgraph_plot %>%
                               sg_annotate(label, x_str(yy), y(yy), 
                                           color=label_color, size=10)
        })
}

# Add annotation for each section
streamgraph_plot <- streamgraph_plot %>%
                    sg_annotate("Critical", "2014-01-01",2000000, color = `@c`(red,0.7)) %>%
                    sg_annotate("Endangered", "2013-05-01",1000000, color = `@c`(red, 0.6 * 0.7)) %>%
                    sg_annotate("Vulnerable", "2013-07-01",200000, color = `@c`(red, 0.3 * 0.7))
        
## ---- end-of-exp-time

## ---- exp-purpose

conservation <- c("Reintroduction To Wild","Breeding")
science <- c("Educational",
             "Scientific",
             "Zoo")
others <- c("Circus/Travelling Exhibition",
            "Law Enforcement")
trades_by_purpose <- dataset %>%
                     mutate(Purpose = 
                              ifelse(is.na(Purpose),"Others/Unknown",
                              ifelse(Purpose %in% science,"Science", 
                              ifelse(Purpose %in% conservation, "Conservation",
                              ifelse(Purpose %in% others, "Others/Unknown", 
                                     Purpose))))) %>%
                     group_by(Purpose) %>%
                     summarise(total_trades = sum(Qty)) %>%
                     mutate(total_trades = as.integer(total_trades)) %>%
                     ungroup()

w_colors <- c("Commercial"=`@c`(red),
              "Hunting"=`@c`(orange),
              "Personal"=`@c`(yellow),
              "Medical"=`@c`(purple),
              "Science"=`@c`(blue),
              "Conservation"=`@c`(green),
              "Others/Unknown"=`@c`(ltxt),
              `@c`(ltxt))
waffle_input <- trades_by_purpose$total_trades
names(waffle_input) <- trades_by_purpose$Purpose
waffle_input <- waffle_input[order(factor(names(waffle_input),levels = names(w_colors)))] %>%
                sapply(function (x) { max(x / sum(waffle_input) * 200,1) })

waffle_plot <- suppressMessages(
                waffle(waffle_input, rows=5, size=1.3) + 
                theme_lk(TRUE, TRUE, FALSE, FALSE) +
                scale_fill_manual(name = "Purpose", values=w_colors) +
                xlab(paste0("1 Square ~ ",
                            comma(round(sum(trades_by_purpose$total_trades) / 200)),
                            " Trades (0.5% of Total)"))
                )
## ---- end-of-exp-purpose

## ---- exp-species

# Kudos to http://timelyportfolio.github.io/sunburstR/example_baseball.html
# for sunburst reference
trades_by_species <- dataset %>%
                     group_by(Class, Order, Family, Genus, Taxon, CommonName) %>%
                     summarise(total_trades = sum(Qty)) %>%
                     ungroup()

sunburst_input <- trades_by_species %>%
                  group_by(Class) %>%
                  mutate(Node = ifelse(is.na(CommonName),Taxon, gsub("-"," ",CommonName)),
                         Category = Class,
                         CategorySize = sum(total_trades),
                         Seq = paste(Class,Node, sep="-"),
                         Depth = 2) %>%
                  ungroup() %>% group_by(Node, Category, CategorySize, Seq, Depth) %>%
                  summarise(Value = sum(total_trades)) %>%
                  ungroup() %>%
                  # Remove any categories than 0.01 percent since they won't appear anyway
                  filter(CategorySize >= 0.0001 * sum(Value))

# Add Categories That Are Not Represented By A Row
additional_nodes <- sunburst_input %>%
                    mutate(Node = Category,
                           Seq = Category,
                           Depth = 1,
                           Value = 0) %>%
                    unique()

sunburst_input <- rbind(sunburst_input,
                        additional_nodes) %>%
                  arrange(Depth, desc(CategorySize), desc(Value))

# Set the colors for each node
sunburst_palette <- `@c`(palette)(length(unique(sunburst_input$Category)))
names(sunburst_palette) <- unique(sunburst_input$Category)
sunburst_cdomain <- sunburst_input$Node
sunburst_crange <- sapply(1:nrow(sunburst_input), 
                          function(i) { 
                            node <- sunburst_input[[i,"Node"]]
                            category <- sunburst_input[[i,"Category"]]
                            color <- sunburst_palette[[category]]
                            opacity <- ifelse(category == node, 0.5,0.8)
                            alpha(color,opacity)
                          })

# Plot!
sunburst_plot <- sunburst(sunburst_input %>% select(Seq, Value) ,
                          colors = list(domain=sunburst_cdomain, range=sunburst_crange),
                          count = TRUE,
                          legend = FALSE,
                          width = 600)

## ---- end-of-exp-species

## ---- exp-imports

# Shape Files Courtesy of 
# http://thematicmapping.org/downloads/world_borders.php
world_borders <- readOGR( dsn= paste0(getwd(),"/",specs_dir,"world_borders") , 
                          layer="TM_WORLD_BORDERS_SIMPL-0.3", 
                          verbose = FALSE)
# Revise some of the country names
world_borders@data <- world_borders@data %>%
                      left_join(read.csv(paste0(specs_dir, "revised_country_names.csv"),sep = "|"), by="NAME") %>%
                      mutate(NAME = ifelse(is.na(UPDATE_NAME),NAME, UPDATE_NAME)) %>%
                      select(-UPDATE_NAME)

# Dataset containing trades_by_country
trades_by_import <- dataset %>%
                    group_by(Importer) %>%
                    summarise(imports = sum(Qty))
trades_by_export <- dataset %>%
                    group_by(Exporter) %>%
                    summarise(exports = sum(Qty))
trades_by_country <- trades_by_import %>%
                      full_join(trades_by_export, by=c("Importer" = "Exporter")) %>%
                      mutate(imports = ifelse(is.na(imports),0,imports),
                             exports = ifelse(is.na(exports),0,exports),
                             net_imports = ifelse(imports > exports, imports - exports, NA),
                             net_exports = ifelse(exports > imports, exports - imports, NA))

get_leaflet_plot <- function(trade_dataset, isImport = TRUE) {
  # Example Plot Courtesy of
  # https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet/
  
  map_unt <- ifelse(isImport,"Net Imports","Net Exports")
  map_col <- ifelse(isImport,"red","purple")
  leg_tit <- ifelse(isImport,"Wildlife Demand by Countries (Quantile)","Wildlife Supply by Countries (Quantile)")
  
  trade_dataset["net_val"] <- ifelse(isImport, trade_dataset["net_imports"], trade_dataset["net_exports"])
  
  # Join trade information into world data
  polygons <- world_borders
  polygons@data <- polygons@data %>%
                   left_join(trade_dataset, by=c("ISO2"="Importer")) %>%
                   mutate(rank = rank(desc(net_val),ties="first"))
  
  # Remove those countries with no wildlife trades
  polygons <- polygons[!is.na(polygons@data$net_val),]
  
  
  # Create leaflet arguments
  # Prevent zooming out infinitely
  leaflet_ptOptions <- providerTileOptions(minZoom = 1)
  # Get colors of each polygon based on trading intensity
  leaflet_palette <- colorQuantile(c(`@c_`(map_col,0.1),`@c_`(map_col)),
                                   polygons$net_val,
                                   n = 5, 
                                   na.color = "#ffffffff")
  # What happens to polygon fill when we hover on it
  leaflet_highlightOptions <- highlightOptions(
                                fillOpacity = 0.5,
                                bringToFront = TRUE)
  # Toppltip to appear on hovering
  leaflet_labels <-  sprintf(
                        paste0("<span style='font-family: var(--heading-family); font-size: 1.2em'>%s</span><br/>",
                               "Ranked <span style='font-size:1.2em'>%s</span> (out of %s)<br/>",
                               "%s %s"),
                        polygons@data$NAME, 
                        sapply(polygons@data$rank, toOrdinal), 
                        length(polygons@data$rank),
                        comma(round(polygons$net_val)), 
                        map_unt) %>% 
                      lapply(htmltools::HTML)
  # Change background color and foreground color based on fill of the hovered area
  leaflet_labelOptions <- lapply(leaflet_palette(polygons$net_val), function (c){ 
                            luminosity <- as(hex2RGB(c), "polarLUV")@coords[1]
                            fg_color <- ifelse(luminosity <= 70, `@c`(bg), `@c`(txt))
                            labelOptions(
                              style = list("background-color" = c,
                                           "font-family" = `@f`,
                                           "font-weight" = "normal", 
                                           "color" = fg_color,
                                           "border-width" = "thin",
                                           "border-color" = fg_color),
                              textsize = "1em",
                              direction = "auto")
                          })
  
  # Markers for Top 5 countries
  marker_data <- polygons[polygons@data$rank <= 5,]
  # Create icons for the Top 5
  marker_icons <- awesomeIcons(
                    # To prevent bootstrap 3.3.7 from loading and removing cosmo theme css, metadata for mobile
                    library = 'fa',
                    markerColor = 'gray',
                    text = sapply(marker_data@data$rank, function(x) { 
                      sprintf("<span style='color: %s; font-size:0.8em'>%s</span>", `@c`(bg), toOrdinal(x)) }),
                    fontFamily = `@f`
                  )
  # Options when marker is clicked
  marker_options <- markerOptions(opacity = 0.9)
  # What to show when marker is clicked
  marker_popup <- sprintf(
                    paste0("<span style='font-family: var(--font-family); color: %s'>[%s] <span style='font-family: var(--heading-family); color: %s; font-size: 1.2em'>%s</span><br/>",
                           "%s %s</span>"),
                    `@c`(txt),
                    sapply(marker_data@data$rank, toOrdinal), 
                    `@c_`(map_col),
                    marker_data@data$NAME, 
                    comma(round(marker_data$net_val)), 
                    map_unt) %>% 
                    lapply(htmltools::HTML)
  
  # Create Leaflet plot
  leaflet(polygons, width = "100%") %>%
    addProviderTiles("CartoDB.Positron",options = leaflet_ptOptions) %>%
    setMaxBounds(-200, 100,200,-100) %>%
    setView(0, 30, 1) %>%
    addPolygons(stroke = FALSE, 
                fillOpacity = 0.8, 
                fillColor = ~leaflet_palette(net_val),
                highlight = leaflet_highlightOptions,
                label = leaflet_labels,
                labelOptions = leaflet_labelOptions) %>%
    addLegend(pal = leaflet_palette, 
              values = ~net_val, 
              opacity = 0.7, 
              title = leg_tit,
              position = "bottomleft") %>%
    addAwesomeMarkers(~LON, ~LAT,
                      data = marker_data,
                      icon = marker_icons,
                      options = marker_options,
                      popup = marker_popup)
}

leaflet_import_plot <- get_leaflet_plot(trades_by_country)

## ---- end-of-exp-imports

## ---- exp-exports
leaflet_export_plot <- get_leaflet_plot(trades_by_country, FALSE)
## ---- end-of-exp-exports

## ---- model-graphs

max_label_char <- 20
vertices <- unique(c(dataset$Importer,dataset$Exporter)) %>%
            sort() %>%
            { data.frame(index=.)} %>%
            inner_join(world_borders@data, by=c("index"="ISO2")) %>%
            # Create labels that truncate names
            mutate(NAME_SHORT = ifelse(nchar(NAME) >= max_label_char, sprintf("%s...",substr(NAME,1,max_label_char - 3)), NAME),
                   REGION = paste0("REGION-",REGION),
                   SUBREGION = paste0("SUBREGION-",SUBREGION)) %>%
            arrange(REGION, SUBREGION)

edges <- dataset %>%
         filter(Importer %in% vertices$index & 
                  Exporter %in% vertices$index &
                  Importer != Exporter) %>%
         mutate(v1 = ifelse(Importer < Exporter, Importer, Exporter),
                v2 = ifelse(Importer < Exporter, Exporter, Importer)) %>%
         group_by(v1,v2) %>%
         summarise(total_trades = sum(Qty))

# Calculate imports + exports of each country
vertices$VALUE <- sapply(vertices$index, function(i) { sum(edges[edges$v1 == i | edges$v2 == i,]$total_trades) })

# Filter Top X Countries If Necessary
input_vertices <- vertices
input_edges <- edges %>% filter(v1 %in% input_vertices$index & v2 %in% input_vertices$index)

# Huge thanks to R Graph Gallery for Template
# https://www.r-graph-gallery.com/hierarchical-edge-bundling/

# Create Inputs For The Model
# Color palette for edges and input_vertices
color_dictionary <- `@c`(palette)(length(unique(input_vertices$REGION)))
names(color_dictionary) <- unique(input_vertices$REGION)
n_points <- 100

# Graph Creation
hierarchy <- rbind(input_vertices %>% mutate(parent="root") %>% select(parent,child=REGION) %>% unique(),
                   input_vertices %>% select(parent=REGION,child=SUBREGION) %>% unique(),
                   input_vertices %>% select(parent=SUBREGION, child=index))

# Node Specifications
nodes <- data.frame(name = unique(c(as.character(hierarchy$parent), as.character(hierarchy$child)))) %>%
         left_join(input_vertices %>% select(index, label=NAME_SHORT, region=REGION, value=VALUE), by=c("name"="index")) %>%
         mutate(ranking=rank(value,ties.method="first"))

# Edge Specifications
connections <- data.frame(from=match(input_edges$v1, nodes$name),
                          to=match(input_edges$v2, nodes$name),
                          weights=input_edges$total_trades)

total_trade_min_cutoff <- quantile(input_edges$total_trades, 0.5)
total_trade_max_cutoff <- quantile(input_edges$total_trades, 0.999)
connections$alpha <- lapply(1:nrow(input_edges), 
                        function(i) {
                          a <- max(min((input_edges$total_trades[i] - total_trade_min_cutoff) / (total_trade_max_cutoff - total_trade_min_cutoff),1.),0.)
                          list(rep(a,n_points))})

connections$colors <- lapply(1:nrow(input_edges), 
                        function(i) {
                          v1 <- input_edges[[i,"v1"]]
                          v2 <- input_edges[[i,"v2"]]
                          
                          # Set up colors
                          region_v1 <- (input_vertices %>% filter(index == v1))$REGION
                          region_v2 <- (input_vertices %>% filter(index == v2))$REGION
                          list(colorRampPalette(c(color_dictionary[[region_v1]],color_dictionary[[region_v2]]))(n_points))
                          
                      })

# Plot the Graph
edge_bundle_graph <-graph_from_data_frame(hierarchy, vertices = nodes)
edge_bundle_plot <- ggraph(edge_bundle_graph, layout="dendrogram", circular=TRUE) +
                    theme_void() +
                    theme_lk(TRUE, TRUE, FALSE, FALSE)

# Add Edge Bundles
# There is a bug in ggraph_1.0.0.9999 where geom_conn_bundle can only plot at most 2 nodes of from before messing up the coloring,
# we will fix this by generating geom_conn_bundle one node at a time
for (v1 in unique(connections$from)) {
  tmp <- connections %>% filter(from == v1)
  edge_bundle_plot <- edge_bundle_plot +        
                      geom_conn_bundle(data=get_con(from = tmp$from, to = tmp$to), 
                                       colour=unlist(tmp$colors),
                                       alpha=unlist(tmp$alpha),
                                       n = n_points,
                                       tension = 0.8) 
  
} 

# Adjust the angle and horizontal alignment of nodes based on the position of x and y
edge_bundle_plot$data["angle"] <- sapply(1:nrow(edge_bundle_plot$data), 
                                       function (i) {
                                          t_ratio <- min(max(edge_bundle_plot$data$y[i] / edge_bundle_plot$data$x[i], -10000),10000)
                                          atan(t_ratio) * 180 / pi
                                        })
edge_bundle_plot$data["hjust"] <- sapply(edge_bundle_plot$data$x, 
                                         function (x) {
                                           ifelse(x < 0, 1, 0)
                                         })
# Add Node Points
edge_bundle_plot <- edge_bundle_plot +
                    # Create Nodes
                    geom_node_point(aes(filter = leaf, x = x, y=y, colour=region, size=value), alpha=0.8) +
                    geom_node_text(aes(filter = leaf, x = x*1.05, y=y*1.05, 
                                       colour=region, label=label, alpha=ranking,
                                       angle=angle, hjust=hjust), size=2.7) +
                    scale_size_continuous(name="Wildlife Trading Activity (Gross Imports + Exports)",
                                          label=function (v) { sprintf("%.0f mil",v/1000000)},
                                          guide=guide_legend(override.aes = list(color=`@c`(txt), alpha=0.8))) + 
                    scale_color_manual(values=color_dictionary, guide="none") +
                    scale_alpha_continuous(limits=c(max(nodes$ranking)-100,max(nodes$ranking)-10), na.value=0.1, guide="none") +
                    # Make sure labels are viewable
                    expand_limits(x = c(-1.30, 1.30), y = c(-1.30, 1.30))
                
## ---- end-of-model-graphs

# Modeling ----
## ---- model-pagerank

trade_graph <- graph_from_data_frame(edges %>% rename(weight=total_trades), vertices = vertices)
pr_rankings <- page_rank(trade_graph, directed = FALSE)$vector
pr_rankings <- data.frame(index=names(pr_rankings), PRANK=pr_rankings)

# Add rankings to the vertices
vertices <- vertices %>%
            left_join(pr_rankings, by="index") %>%
            mutate(R_PRANK = rank(desc(PRANK),ties.method="first"))

## ---- end-of-model-pagerank

## ---- model-pagerank-hist

pr_inputs <- vertices %>%
              mutate(LOG_PRANK = log(PRANK),
                     LOG_VALUE = log(VALUE),
                     # Z value to normalize both
                     Z_PRANK = (LOG_PRANK - mean(LOG_PRANK)) / sd(LOG_PRANK),
                     Z_VAL = (LOG_VALUE - mean(LOG_VALUE)) / sd(LOG_VALUE),
                     # Rankings
                     R_VAL = rank(desc(VALUE), ties.method="first"),
                     R_DIFF = R_PRANK - R_VAL,
                     HAS_DIFF = ifelse(R_DIFF == 0,"NONE",REGION))

hist_input <- select(pr_inputs, Z_VAL, Z_PRANK) %>%
              gather("type","val") %>%
              arrange(desc(type))

hist_plot <- ggplot(hist_input) + 
             theme_lk() +
             theme(
               axis.ticks.x = element_blank(),
               axis.title.x = element_blank(),
               axis.text.x = element_blank(),
               axis.line.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.title.y = element_blank(),
               axis.text.y = element_blank()
             ) +
             scale_x_continuous(expand=c(0,0)) + 
             scale_y_continuous(expand=c(0,0)) +
             # Add Mean line
             geom_vline(data=data.frame(1), xintercept = 0, linetype="dashed", colour=`@c`(ltxt)) + 
             # Add descriptors
             geom_text(data=data.frame(x=c(0.1,-3,7),
                                        y=c(0.9,0.1,0.1),
                                        hjust=c(0,0,1),
                                        label=c("Mean","Less Important","More Important")),
                        aes(x=x, y=y, hjust=hjust, label=label),
                        colour = `@c`(ltxt),
                        family = `@f`) + 
             # Add geom density
             geom_density(aes(x=val, fill=type, colour=type), alpha=0.25) +
             scale_fill_manual(name="Methodology", values=`@c`()[1:2], 
                               labels=c("Z_VAL" = "Imports + Exports",
                                        "Z_PRANK" = "PageRank"),
                               guide=guide_legend(override.aes = list(color = `@c`()[1:2]))) +
             scale_color_manual(guide="none", values=`@c`()[1:2])

## ---- end-of-model-pagerank-hist

## ---- model-pagerank-rank

max_rank <- 25
rank_plot <- ggplot(pr_inputs, aes(colour=HAS_DIFF)) + 
              theme_void() +
              theme_lk(TRUE, FALSE, FALSE, FALSE) +
              # Prevent removing points or arrows outside the limit
              coord_cartesian(xlim = c(-2.0,max_rank + 0.5), ylim=c(-1.0,1.7)) +
              # Modify scales
              scale_x_continuous(expand=c(0,0.05)) +
              scale_y_continuous(expand=c(0,0.05)) +
              scale_color_manual(values=c(color_dictionary,"NONE"=`@c`(txt,0.2)),
                                 guide="none") +
              scale_size_continuous(guide="none") +
              # Add descriptors
              geom_text(data=data.frame(x=c(0.2,0.2,1.0,max_rank),
                                        y=c(0,1,-0.9,-0.9),
                                        hjust=c(1,1,0,1),
                                        label=c("Imports\n+ Exports","PageRank",
                                                "More Important","Less Important")),
                        aes(x=x, y=y, hjust=hjust, label=label),
                        colour = `@c`(ltxt),
                        family = `@f`, 
                        size = 3) +
              geom_segment(data=data.frame(1),
                           aes(x=max_rank, xend=0.5,y=-0.8,yend=-0.8),
                           colour = `@c`(ltxt),
                           arrow = arrow(length = unit(0.2,"cm"), type="closed")) +
              # Add imports + exports ranks
              geom_text(aes(x=R_VAL, y=-0.1, label=NAME_SHORT), angle=90, hjust=1, size = 3) +
              geom_point(aes(x=R_VAL, y=0, size=Z_VAL), alpha=0.5) +
              # Add pagerank probabilities
              geom_text(aes(x=R_PRANK, y=1.1, label=NAME_SHORT), angle=90, hjust=0, size = 3) + 
              geom_point(aes(x=R_PRANK, y=1.0, size=Z_PRANK), alpha=0.5) +
              # Add arrows
              geom_segment(aes(x=R_VAL, xend=R_PRANK, y=0.1, yend=0.9),
                           arrow = arrow(length = unit(0.2, "cm"), type="closed")) 

## ---- end-of-model-pagerank-rank

## ---- model-pagerank-compare

# Get edges only for neighbors of the two countries of interest
c_int <- sort(c("JP","EC"))
tc_edges <- edges %>%
            filter(v1 %in% c_int | v2 %in% c_int) %>%
            filter(!(v1 == c_int[1] & v2 == c_int[2]))

# Get only neighboring vertices
nodes_int <- unique(c(tc_edges$v1, tc_edges$v2))
V1_vertice <- (vertices %>% filter(index == c_int[1]))
V2_vertice <- (vertices %>% filter(index == c_int[2]))
tc_vertices <- vertices %>% filter(index %in% nodes_int & !(index %in% c_int))

# Construct Input
circ_input <- tc_vertices %>%
              mutate(x = rank(desc(PRANK),ties.method="first"),
                     label = NAME_SHORT,
                     theta = 90 - x / nrow(tc_vertices) * 360)
circ_input$hjust <- sapply(circ_input$theta, function(t) { if (t <= -90){1} else {0}})
circ_input$theta <- sapply(circ_input$theta, function (t) { if (t <= -90) { t + 180} else {t}})

# Colors
neighbors_V1 <- (tc_edges %>% filter(v1 == c_int[1] | v2 == c_int[1]) %>% mutate(v = ifelse(v1 == c_int[1],v2,v1)))$v
neighbors_V2 <- (tc_edges %>% filter(v1 == c_int[2] | v2 == c_int[2]) %>% mutate(v = ifelse(v1 == c_int[2],v2,v1)))$v
circ_input$tag <- sapply(tc_vertices$index, function (ix) {
                    if (ix %in% neighbors_V1 & ix %in% neighbors_V2) { "BOTH" }
                    else if (ix %in% neighbors_V1) { c_int[1] }
                    else if (ix %in% neighbors_V2) { c_int[2] }
                    else { "NONE" }
                  })

compare_colors <- c(`@c`(txt,0.2), color_dictionary[[V1_vertice$REGION]],color_dictionary[[V2_vertice$REGION]])
names(compare_colors) <- c("BOTH", c_int)
compare_labels <- c("Both",V1_vertice$NAME,V2_vertice$NAME)
names(compare_labels) <- c("BOTH",c_int)
compare_plot <- ggplot(circ_input, aes(x=x, y=1, size=PRANK, color=tag)) +
                theme_void() +
                theme_lk(TRUE, TRUE, FALSE, FALSE) +
                theme(plot.margin = unit(c(20,0,20,0),'pt')) +
                expand_limits(y = c(0, 1.30)) +
                # Add points, lines and labels of neighboring countries
                geom_point(alpha=0.5) + 
                geom_segment(data= circ_input %>% filter(tag %in% c_int), 
                             aes(xend=x, y=0, yend=1, alpha=VALUE), 
                             size=0.5) +
                geom_text(data= circ_input %>% filter(tag %in% c_int),
                          aes(label=label, angle=theta, hjust=hjust), 
                          y = 1.05, 
                          size=3,
                          show.legend = FALSE) +
                # Change Legends
                scale_size_continuous(name = "PageRank",
                                      labels = percent) +
                scale_color_manual(name="Traded With",
                                   values = compare_colors,
                                   labels = compare_labels) +
                scale_alpha_continuous(guide="none") +
                # Change to coordinate polar
                coord_polar()

## ---- end-of-model-pagerank-compare

## ---- model-pagerank-decompose-overview

# Calculate the Net Exports and Imports for each country at a specified level of taxonomy granularity
net_gross_p_country <- function(granularity="Taxon") {
  
  imports <- dataset %>% 
    group_by_at(vars("Importer", granularity)) %>% 
    summarise(Imports = sum(Qty)) %>% ungroup()
  exports <- dataset %>% 
    group_by_at(vars("Exporter", granularity)) %>% 
    summarise(Exports = sum(Qty)) %>% ungroup()
  
  by_joins <- c("Exporter",granularity)
  names(by_joins) <- c("Importer",granularity)

  trades <- imports %>% 
    full_join(exports, by=by_joins) %>%
    mutate(
      index = Importer,
      Imports = ifelse(is.na(Imports),0,Imports),
      Exports = ifelse(is.na(Exports),0,Exports),
      Gross = Imports + Exports,
      Net_Exports = ifelse(Exports > Imports, Exports - Imports,0),
      Net_Imports = ifelse(Imports > Exports, Imports - Exports,0),
      # Trades that are not supplied [net export] or consumed [net imports] are ones
      # that are being transited within the country
      Transits = ifelse(Imports > Exports, Exports, Imports)
    )
  trades %>% select_at(vars("index",granularity,"Imports","Exports","Gross","Net_Exports","Net_Imports","Transits"))
}
country_taxon_trades <- net_gross_p_country()

# Calculate the demand, supply and dealer rankings for each country using the following:
# ConsumerRank: net imports / gross * PageRank
# SupplierRank: net exports / gross * PageRank
# DealerRank: transits / gross * PageRank
country_add_info <- country_taxon_trades %>%
                      group_by(index) %>%
                      summarise(GROSS = sum(Gross),
                                NET_IMPORTS = sum(Net_Imports),
                                NET_EXPORTS = sum(Net_Exports),
                                TRANSITS = sum(Transits))

vertices <- vertices %>%
            left_join(country_add_info, country_add_info, by="index") %>%
            mutate(
              CRANK = NET_IMPORTS / GROSS * PRANK,
              R_CRANK = rank(desc(CRANK), ties.method="first"),
              DRANK = TRANSITS / GROSS * PRANK,
              R_DRANK = rank(desc(DRANK), ties.method="first"),
              SRANK = NET_EXPORTS / GROSS * PRANK,
              R_SRANK = rank(desc(SRANK), ties.method="first")
            )

## ---- end-of-model-pagerank-decompose-overview

## ---- model-pagerank-decompose-leaf

# Normalize each of the ranks so that totals of each rank = 1
leaf_inputs <- vertices %>%
                mutate(
                  CRANK = CRANK / sum(CRANK),
                  DRANK = DRANK / sum(DRANK),
                  SRANK = SRANK / sum(SRANK)
                )

# Get only the top countries to show degree of roles
n_show <- 12
leaf_inputs <- leaf_inputs %>% arrange(desc(PRANK)) %>% head(n_show)
leaf_inputs$NAME <- factor(leaf_inputs$NAME, levels=unique(leaf_inputs$NAME))

# Normalize between the three ranks to make sure that plot is properly "zoomed"
# as free_y is not available for polar coordinates
leaf_inputs$MAXR <- sapply(1:nrow(leaf_inputs), function(i) {
  max(leaf_inputs[i,"CRANK"],leaf_inputs[i,"DRANK"],leaf_inputs[i,"SRANK"])
})
leaf_inputs <- leaf_inputs %>%
                mutate(
                  CRANK = CRANK / MAXR,
                  DRANK = DRANK / MAXR,
                  SRANK = SRANK / MAXR,
                  CLOSE = CRANK
                ) %>%
                gather("Key","Value", CRANK, DRANK, SRANK, CLOSE) %>%
                mutate(
                  Key = ifelse(Key == "CRANK",0,
                               ifelse(Key == "DRANK",120,
                                      ifelse(Key == "SRANK",240,
                                             ifelse(Key == "CLOSE",360,0))))
                ) %>%
                select(NAME, REGION, PRANK, Key, Value)

# Plot!
leaf_plot <- ggplot(leaf_inputs, aes(x=Key, y=Value, fill=REGION, color=REGION)) + 
              coord_polar(start=60/180*pi) +
              theme_lk(TRUE, TRUE, FALSE, FALSE) + 
              theme(
                axis.line.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.x = element_blank(),
                axis.text.x = element_text(colour=`@c`(ltxt), size = 8, angle=c(-60,0,60)),
                panel.grid.major.x = element_line(colour=`@c`(txt,0.2)),
                axis.line.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                panel.grid.major.y = element_blank(),
                strip.background = element_blank(),
                strip.text.x = element_blank()
              ) + 
              scale_x_continuous(limits=c(0,360), 
                                 breaks = c(0,120,240), 
                                 labels=c("Consumer","Dealer","Supplier"),
                                 minor_breaks = NULL) +
              scale_y_continuous(limits=c(0,NA)) +
              scale_fill_manual(values = color_dictionary, guide = "none") + 
              scale_color_manual(values = color_dictionary,guide = "none") +
              geom_polygon(alpha=0.2, size=0) + 
              geom_text(data=leaf_inputs %>% select(NAME, REGION) %>% unique(),
                        aes(label=NAME),
                        x=0,y=0, 
                        family=`@f`,
                        alpha=0.9) + 
              facet_wrap(~NAME)

## ---- end-of-model-pagerank-decompose-leaf

## ---- model-results

to_text <- function (data) {
  tmp <- data %>% head(5)
  tmp$NAME
}

consumers <- vertices %>% arrange(R_CRANK)
dealers <- vertices %>% arrange(R_DRANK)
suppliers <- vertices %>% arrange(R_SRANK)

players <- data.frame("Consumers"=to_text(consumers),
                      "Dealers"=to_text(dealers),
                      "Suppliers"=to_text(suppliers))

## ---- end-of-model-results

## ---- results

get_players <- function(countries, col, type, disclaimers=c()) {
  
  c_container <- '<div class="players-container">'
  
  for (c in countries) {
    c_row <- vertices %>% filter(index == c)
    
    # Create Title
    c_title <- sprintf("<span style='color: %s'>%s</span>",
                      col,
                      c_row$NAME)
    
    # Create Body
    # 1st get pagerank values
    get_rank_text <- function(rankName, rankCol) {
      sprintf("<span style='font-size:0.8em'>%s:</span> <span class='hl' style='color: %s'>%s</span><br>",
              rankName,
              col,
              toOrdinal(c_row[[paste0("R_",rankCol)]])
              )
    }
    c_body <- get_rank_text("PageRank","PRANK") %>%
              paste0(get_rank_text("ConsumerRank","CRANK")) %>%
              paste0(get_rank_text("DealerRank","DRANK")) %>%
              paste0(get_rank_text("SupplierRank","SRANK"))
    
    # 2nd get imports and exports
    c_ie <- trades_by_country %>% filter(Importer == c)
    c_body <- paste0(c_body, 
                     sprintf("<span style='font-size:0.8em'>Imports/Exports:</span> <span class='hl' style='color: %s'>%s/%s</span><br><br>", 
                             col,
                             comma_format(0)(c_ie$imports), 
                             comma_format(0)(c_ie$exports)))
    
    # 3rd get most commonly traded species
    c_animals <- country_taxon_trades %>%
                 filter(index == c) %>%
                 left_join(unique(select(dataset, Taxon, CommonName)), by="Taxon") %>%
                 mutate(Name = ifelse(is.na(CommonName), Taxon, CommonName)) %>%
                 arrange_at(vars(type),desc) %>%
                 head(5)

    c_body <- paste0(c_body,
                     sprintf("<span style='font-size:0.8em'>Top %s Species [%s]</span>:<br>",
                             ifelse(type=="Net_Imports", "Imported", 
                             ifelse(type=="Net_Exports","Exported","Transit")),
                             ifelse(type %in% c("Net_Imports","Net_Exports"), gsub("_"," ",type), "Quantity")))
    
    c_disclaimer <- ""
    for (i in 1:nrow(c_animals)) {
      animal_name <- c_animals[[i,"Name"]]
      animal_count <- c_animals[[i,type]]
      
      has_disclaimer <- animal_name %in% names(disclaimers[[c]])
      disclaimer_index <- ifelse(has_disclaimer, match(animal_name, names(disclaimers[[c]])), "")
      
      c_body <- paste0(c_body,
                       sprintf("%s<sup>%s</sup> [%s]<br>",
                               animal_name,
                               disclaimer_index,
                               comma_format(0)(animal_count)))
      
      # Specify disclaimers if any
      if (has_disclaimer) {
        c_disclaimer <- paste0(c_disclaimer,
                             sprintf("<sup>%s</sup>%s<br>",
                                disclaimer_index,
                                disclaimers[[c]][animal_name]))
      }
    }
    
    # 4th Add disclaimers if there's any
    if (c_disclaimer != "") {
      c_body <- paste0(c_body,"<br><div style='font-size:0.7em; line-height: 1.15;'>",c_disclaimer,"</div>")
    }
    
    c_html <- sprintf(
                paste0(
                  '<a class="players" data-toggle="popover" data-html="true" title="" data-content="%s" data-original-title="%s">',
                    '<img src="img/%s.png">',
                  '</a>'),
                c_body,
                c_title,
                tolower(c),
                col
              )
    
    c_container <- paste0(c_container,
                          c_html)
                 
  }
  
  c_container <- paste0(c_container, "</div>",
                        '<div style="font-size:0.9em; color: var(--font-color-75); text-align:right; padding:1em">Icons courtesy of <a href="http://www.customicondesign.com/free-icons/flag-icon-set/flat-round-world-flag-icon-set/" target="_blank">Custom Icon Design</a></div>')
  
  c_container
}

## ---- end-of-results


                                                  
