### Find this work and others at my website [[lemuelkumarga.com]](https://www.lemuelkumarga.com/#github)!
---



## Package Requirements

### Mac/Ubuntu Operating System
- Other OS-es have not been tested, and may cause unexpected errors.

### Pandoc 2.1 
- Need to reference googlefonts

### Git LFS
- If there exists a cache folder, you may need to install Git LFS
to pull files > 100 MB. Please follow instructions [here](https://github.com/git-lfs/git-lfs/wiki/Installation).
- After Git LFS is installed, type `git lfs pull` to download the large files in the project.


## Cloning the Repository

Cloning the repository is not as straightforward due to the presence of git submodules.

Please replicate the steps below in Terminal to ensure success.

``` sh
# Clone the repo as usual
git clone https://github.com/lemuelkumarga/wildlife-trade

# Initialize submodule
cd wildlife-trade
git submodule init
git submodule update

# When cloned, submodules are detached from the HEAD. We attempt to rectify this issue to prevent problems in git
cd shared
git checkout -b tmp
git checkout master
git merge tmp
git branch -d tmp

# Return to original folder if desired
cd ../../
```

---
Identifying Major Players in Endangered Wildlife Trade
================
<span class="meta">Lemuel Kumarga</span>
<span class="meta">May 2018</span>

## Problem Description

Since the Stone Age, mankind has turned to nature for food, commerce and
companionship. Our impact back then was minimal as resources were
consumed at sustainable rates. However, over the past few decades,
nature is increasingly under threat from overconsumption and excess
capitalism. Our planet is facing multiple challenges on the
environmental front, ranging from global warming to forest degradation.

One corrosive impact of our greed is the shrinking wildlife diversity
and population. In 2000, IUCN counted a total of around
<a href="https://portals.iucn.org/library/sites/library/files/documents/RL-2000-001.pdf" target="_blank">10,000
endangered species</a>. By 2017, this number has doubled to
<a href="https://www.statista.com/statistics/269910/red-list-endangered-animals-2010-and-2000/" target="_blank">24,431
species</a>. In the face of such overwhelming statistics, it is
imperative for us to take action and protect these species from
extinction. In this project, we will aim to contribute by using data
from both
<a href="https://www.cites.org/eng/disc/what.php" target="_blank">CITES
(Convention on Internation Trade in Endangered Species of Wild Fauna and
Flora)</a> and
<a href="http://www.iucnredlist.org/about/introduction" target="_blank">IUCN</a>
<span class="hl">to identify the major players behind endangered
wildlife trade</span>.

To skip the methodology and proceed straight to the results, please
click <a href="#summary-of-results">here</a>.

## Preliminaries

First load the necessary packages for this exercise.

``` r
# Load default settings for R Markdown -- see file for more details
source("shared/defaults.R")
# Load some helper functions
source("shared/helper.R")

options(stringsAsFactors = FALSE)
packages <- c("dplyr","ggplot2","tidyr","pander","DiagrammeR")
load_or_install.packages(packages)

data_dir <- "data/"
specs_dir <- "specs/"

si <- sessionInfo()
base_pkg_str <- paste0("Base Packages: ",paste(si[["basePkgs"]], collapse=", "))
attached_pkg_str <- paste0("Attached Packages: ",paste(names(si[["otherPkgs"]]), collapse=", "))
cat(paste0(base_pkg_str,"\n",attached_pkg_str))
```

    ## Base Packages: stats, graphics, grDevices, utils, datasets, methods, base
    ## Attached Packages: bindrcpp, DiagrammeR, tidyr, pander, ggplot2, dplyr, knitr

## About the Data

We will be using wildlife trade data from CITES for the period of 2001
to 2015, with the following caveats:

  - 2016 and 2017 were excluded due to data lag in the year of analysis
    (2018). (See
    <a href="https://trade.cites.org/cites_trade_guidelines/en-CITES_Trade_Database_Guide.pdf" target="_blank">Section
    1.2.2 of the guide</a> for more details.)
  - Analysis will be restricted to trades whose sources originated from
    the wild.

Listed below is an overview of the CITES data:

``` r
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
  
pander(cols_summary, caption='Wildlife Trade Data - For more info, please visit <a href="https://trade.cites.org/" target="_blank">CITES Trade Database</a>')
```

| ColumnNames                | Type      | Examples                                                                                                 | PctFilled |
| :------------------------- | :-------- | :------------------------------------------------------------------------------------------------------- | :-------- |
| Year                       | INTEGER   | 2001 // 2002 // 2003 // 2004 // 2005                                                                     | 100%      |
| App.                       | CHARACTER | I // II // III // N                                                                                      | 100%      |
| Taxon                      | CHARACTER | Aquila heliaca // Haliaeetus albicilla // Haliaeetus leucocephalus // Harpia harpyja // Acipenser sturio | 100%      |
| Class                      | CHARACTER | Aves // Actinopteri // Mammalia // Reptilia // Amphibia                                                  | 93%       |
| Order                      | CHARACTER | Falconiformes // Acipenseriformes // Anseriformes // Pinales // Primates                                 | 99%       |
| Family                     | CHARACTER | Accipitridae // Acipenseridae // Anatidae // Araucariaceae // Atelidae                                   | 98%       |
| Genus                      | CHARACTER | Aquila // Haliaeetus // Harpia // Acipenser // Branta                                                    | 97%       |
| Importer                   | CHARACTER | US // AT // GL // CA // CL                                                                               | 99%       |
| Exporter                   | CHARACTER | KZ // DK // US // CA // PT                                                                               | 98%       |
| Origin                     | CHARACTER | GL // US // BR // EC // PA                                                                               | 44%       |
| Importer.reported.quantity | NUMERIC   | 100 // 46 // 1 // 33 // 188                                                                              | 50%       |
| Exporter.reported.quantity | NUMERIC   | 57 // 1 // 12 // 89 // 6                                                                                 | 72%       |
| Term                       | CHARACTER | specimens // live // bodies // feathers // claws                                                         | 100%      |
| Unit                       | CHARACTER | ml // kg // g // sets // flasks                                                                          | 11%       |
| Purpose                    | CHARACTER | Scientific // Personal // Zoo // Law Enforcement // Hunting                                              | 96%       |
| Source                     | CHARACTER | Wild // Unknown                                                                                          | 97%       |

Wildlife Trade Data - For more info, please visit
<a href="https://trade.cites.org/" target="_blank">CITES Trade
Database</a>

From the
<a href="https://trade.cites.org/cites_trade_guidelines/en-CITES_Trade_Database_Guide.pdf" target="_blank">guide</a>
and the above summary, we know that:

1.  Each row corresponds to the <span class="hl">total trade</span>
    between two countries for a particular species at a particular term.
    This is contrary to popular belief that each row corresponds to one
    shipment. (See Section 3.1 for more details.)
2.  Not all animals in the data are endangered. For example, the
    <a href="https://en.wikipedia.org/wiki/White-tailed_eagle" target="_blank">white-tailed
    eagle (Haliaeetus albicilla)</a> is specified as
    <span class="hl">Least Concern</span> on the IUCN Red List.
3.  Terms are <span class="hl">heterogenous</span>. For example, some
    quantities correspond to bodies, while others correspond to
    feathers.
4.  Units are also <span class="hl">varied</span>. Quantities can be
    quoted as distinct counts (i.e. blank unit), or in terms of
    weight/volume/qualitative units.

As can be seen, some pre-processing would be required before our
analysis can proceed. In particular, (3) and (4) need to be standardized
to allow comparison across species.

## Pre-Processing

### Restricting the Species

#### Animals Only

In the data, taxonomies with blank <span class="hl">Classes</span> are
plants. For the purposes of this analysis, these records (or rows) will
be excluded from the dataset.

``` r
pre_clean <- nrow(dataset)
post_clean_str <- function () {
  post_clean <- nrow(dataset)
  cat(sprintf("%i rows removed (%i%% of total)",pre_clean - post_clean,floor((pre_clean - post_clean)/pre_clean * 100)))  
}

dataset <- dataset %>%
  filter(Class != "")

post_clean_str()
```

    ## 31068 rows removed (6% of total)

#### Endangered Only

Endangered status of species are located in the IUCN Red List database.
Due to licensing restrictions, the data could not be uploaded for public
view. However, one is free to create an account and download the csv
from
<a href="http://www.iucnredlist.org/search/saved?id=90695" target="_blank">here</a>.

By integrating the IUCN data, we can subsequently filter non-endangered
species out of the CITES
dataset.

``` r
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

post_clean_str()
```

    ## 349257 rows removed (76% of total)

### Standardizing the Terms

After restricting the dataset to only endangered species, we need to
standardize all the terms below into universal <span class="hl">animal
units</span>:

``` r
# Take the majority between input and output
dataset <- dataset %>%
  mutate(Qty = ifelse(is.na(Importer.reported.quantity),Exporter.reported.quantity,
                      ifelse(is.na(Exporter.reported.quantity),Importer.reported.quantity,
                             ifelse(Exporter.reported.quantity > Importer.reported.quantity,
                                    Exporter.reported.quantity,
                                    Importer.reported.quantity))))

output_str <- "List of Terms:\n" %>%
               paste0(paste0(unique(dataset$Term), collapse=", "),"\n\n") %>%
               paste0("List of Units:\n") %>%
               paste0(paste0(unique((dataset %>% 
                       mutate(Unit = ifelse(Unit == "","count",Unit)))$Unit), 
                       collapse=", "))

cat(output_str)
```

    ## List of Terms:
    ## specimens, derivatives, unspecified, bones, skin pieces, skins, horns, skulls, trophies, feathers, live, skeletons, hair, eggs (live), bodies, carapaces, scales, carvings, shoes, meat, leather products (small), eggs, leather products (large), claws, teeth, tusks, ivory carvings, ivory pieces, hair products, feet, ears, sets of piano keys, tails, bone pieces, garments, bone carvings, plates, extract, wax, oil, horn carvings, shells, gall, swim bladders, raw corals, leather items, genitalia, musk, powder, gall bladders, coral sand, horn pieces, venom, ivory scraps, fins, baleen, caviar, calipee, skin scraps, fingerlings, medicine, sides, jewellery - ivory , cloth, cosmetics, trunk, jewellery, rug, piano keys, fur products (large), dried plants
    ## 
    ## List of Units:
    ## count, ml, g, kg, flasks, mg, sets, cm, pairs, bags, cans, ft2, cartons, m2, boxes, l, cm2, bottles, m, shipments, cm3, m3, sides, microgrammes

#### Scientific Units

The first thing to note is that not all of the
<span class="hl">units</span> are SI (e.g. <span class="hl">cm, g and
litres</span>). This is relatively straightforward to fix:

``` r
pre_clean <- nrow(dataset %>% group_by(Term, Unit) %>% summarise())
post_clean_str <- function () {
  post_clean <- nrow(dataset %>% group_by(Term, Unit) %>% summarise())
  cat(sprintf("%i term-unit pair%s remaining (%i%% of total removed)",
              post_clean,
              ifelse(post_clean == 1,"","s"),
              floor((pre_clean - post_clean)/pre_clean * 100)))
}

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

for (i in which(dataset$Unit %in% names(units_to_si))) {
  qty <- dataset[i,"Qty"]
  unit <- dataset[i,"Unit"]
  dataset[i,"Unit"] <- units_to_si[[unit]][1]
  dataset[i,"Qty"] <- as.double(units_to_si[[unit]][2]) * qty
}

post_clean_str()
```

    ## 161 term-unit pairs remaining (27% of total removed)

#### One Term, One Unit

Another issue is that one term can be recorded in different units.
Consider the term <span class="hl">bodies</span>, which were recorded in
counts, kilograms and even metres:

``` r
term_unit_counts <- dataset %>%
  group_by(Term,Unit) %>%
  summarise(Records = n(),
            Quantity = sum(Qty))
  
output_tbl <- term_unit_counts %>%
              filter(Term == "bodies")

pander(output_tbl)
```

| Term   | Unit | Records | Quantity |
| :----- | :--- | :-----: | :------: |
| bodies |      |  1604   | 5487551  |
| bodies | kg   |   360   |  334071  |
| bodies | m    |    1    |   0.1    |

Ideally, we would convert kilograms of bodies into actual count by
identifying species’ weight. However, such methods are time-intensive,
and fall outside the scope of this analysis. Therefore, we need to
propose a conversion rule that is more manageable and yet still
relatively accurate.

To standardize units for each term, we will first choose the target unit
to convert to. This can be done by identifying the unit with the highest
number of records.

``` r
target_unit <- term_unit_counts %>%
  ungroup() %>%
  group_by(Term) %>%
  mutate(r = rank(desc(Records), ties.method = 'first')) %>%
  summarise(
    NumberOfUnits = n(),
    Unit = max(ifelse(r == 1, Unit, NA), na.rm=TRUE),
    Records = max(ifelse(r == 1,Records,NA), na.rm=TRUE)
  )
```

The tricky part comes in when we attempt to convert a quantity from a
non-target unit to the target unit. During this step, we will assume
that, <span class="hl">for each species, the median quantity traded in
animal units is constant regardless of the terms/units they were
specified in</span>. In other words, if the median quantity of elephant
bodies traded is 2, and the median quantity of elephant bodies traded in
kgs is 14,000, then the 2 elephant bodies are equivalent to 14,000 kgs.

This assumption allows us to convert quantities to the target unit using
the following equation:

\[ q_{t} = \frac{q_{nt}}{m_{nt}} \times m_{t} \]

where <br> \(q_{t}\) corresponds to the quantity traded (in target
unit), <br> \(q_{nt}\) corresponds to the quantity traded (in non-target
unit), <br> \(m_{nt}\) corresponds to the median quantity traded (in
non-target unit), and <br> \(m_{t}\) corresponds to the median quantity
traded (in target unit).

##### The Roll-Up Median Approach

One potential challenge with the above approach is the lack of data
points for each species, term and unit triplet. To illustrate, consider
the African bush elephant (Loxodonta africana) trades below:

``` r
output_tbl <- dataset %>% 
              filter(Taxon == "Loxodonta africana") %>%
              left_join(target_unit, by=c("Term","Unit")) %>%
              filter(is.na(NumberOfUnits)) %>%
              group_by(Taxon, Term, Unit) %>%
              summarise(Records = n())

pander(head(output_tbl,5))
```

| Taxon              | Term        | Unit | Records |
| :----------------- | :---------- | :--- | :-----: |
| Loxodonta africana | bodies      | kg   |    1    |
| Loxodonta africana | bones       | kg   |    1    |
| Loxodonta africana | carvings    | kg   |   20    |
| Loxodonta africana | carvings    | m3   |    1    |
| Loxodonta africana | derivatives | kg   |    4    |

There is only a <span class="hl">single</span> data point whose term and
unit are “bodies” and “kg”\! Such minute sample size is not sufficient
to calculate the median. To obtain larger sample sizes, a roll-up
approach is
required:

``` r
# For documentation, please visit http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html
grViz(paste0("
  digraph RollUp {

    # Default Specs
    graph [compound = true, nodesep = .5, ranksep = .25]
    node [fontname = '",def_font,"', fontsize = 14, fontcolor = '",bg_color,"', penwidth=0, color='",ltxt_color,"', style=filled]
    edge [fontname = '",def_font,"', fontcolor = '",ltxt_color,"', color='",ltxt_color,"']
    
    # Input Specs
    Inp [fillcolor = '",ltxt_color,"', label = '(Species s, Term t, Unit u)', shape = rectangle]

    # Conclude Specs
    node [shape = oval]
    Species_Y [label = 'Use the median quantity\nof Species s, Term t and Unit u.', fillcolor = '",get_color(1),"']
    Genus_Y [label = 'Use the median quantity\nof Genus g, Term t and Unit u.', fillcolor = '",get_color(2),"']
    Family_Y [label = 'Use the median quantity\nof Family f, Term t and Unit u.', fillcolor = '",get_color(3),"']
    Order_Y [label = 'Use the median quantity\nof Order o, Term t and Unit u.', fillcolor = '",get_color(4),"']
    Class_Y [label = 'Use the median quantity\nof Class c, Term t and Unit u.', fillcolor = '",get_color(5),"']
    Kingdom_Y [label = 'Use the median quantity\nof Term t and Unit u (across all).', fillcolor = '",txt_color,"']

    # Trigger Specs
    node [shape = diamond, fillcolor = '",bg_color,"', fontcolor = '",txt_color,"', penwidth = 1]
    Species_T [label = 'Does the Species s\nhave >=10 records\nfor the term t and unit u?']
    { rank = same; Species_T, Species_Y }
    Genus_T [label = 'Does g (the Genus of s)\nhave >=10 records\nfor the term t and unit u?']
    { rank = same; Genus_T, Genus_Y }
    Family_T [label = 'Does f (the Family of g)\nhave >=10 records\nfor the term t and unit u?']
    { rank = same; Family_T, Family_Y }
    Order_T [label = 'Does o (the Order of f)\nhave >=10 records\nfor the term t and unit u?']
    { rank = same; Order_T, Order_Y }
    Class_T [label = 'Does c (the Class of o)\nhave any record\nfor the term t and unit u?']
    { rank = same; Class_T, Class_Y }

    # Yes edges
    Inp -> Species_T
    edge [arrowhead = 'box', label = 'Yes']
    Species_T -> Species_Y
    Genus_T -> Genus_Y
    Family_T -> Family_Y
    Order_T -> Order_Y
    Class_T -> Class_Y
    
    # No edges
    edge [label = '     No']
    Species_T -> Genus_T
    Genus_T -> Family_T
    Family_T -> Order_T
    Order_T -> Class_T
    Class_T -> Kingdom_Y

  }
"), height=700)
```

<!--html_preserve-->

<div id="htmlwidget-439b05d926e864897148" class="grViz html-widget" style="width:672px;height:700px;">

</div>

<script type="application/json" data-for="htmlwidget-439b05d926e864897148">{"x":{"diagram":"\n  digraph RollUp {\n\n    # Default Specs\n    graph [compound = true, nodesep = .5, ranksep = .25]\n    node [fontname = \"Source Sans Pro\", fontsize = 14, fontcolor = \"#ffffff\", penwidth=0, color=\"#424242BF\", style=filled]\n    edge [fontname = \"Source Sans Pro\", fontcolor = \"#424242BF\", color=\"#424242BF\"]\n    \n    # Input Specs\n    Inp [fillcolor = \"#424242BF\", label = \"(Species s, Term t, Unit u)\", shape = rectangle]\n\n    # Conclude Specs\n    node [shape = oval]\n    Species_Y [label = \"Use the median quantity\nof Species s, Term t and Unit u.\", fillcolor = \"#335A87\"]\n    Genus_Y [label = \"Use the median quantity\nof Genus g, Term t and Unit u.\", fillcolor = \"#3C7759\"]\n    Family_Y [label = \"Use the median quantity\nof Family f, Term t and Unit u.\", fillcolor = \"#A43820\"]\n    Order_Y [label = \"Use the median quantity\nof Order o, Term t and Unit u.\", fillcolor = \"#5C3C7C\"]\n    Class_Y [label = \"Use the median quantity\nof Class c, Term t and Unit u.\", fillcolor = \"#377D95\"]\n    Kingdom_Y [label = \"Use the median quantity\nof Term t and Unit u (across all).\", fillcolor = \"#424242\"]\n\n    # Trigger Specs\n    node [shape = diamond, fillcolor = \"#ffffff\", fontcolor = \"#424242\", penwidth = 1]\n    Species_T [label = \"Does the Species s\nhave >=10 records\nfor the term t and unit u?\"]\n    { rank = same; Species_T, Species_Y }\n    Genus_T [label = \"Does g (the Genus of s)\nhave >=10 records\nfor the term t and unit u?\"]\n    { rank = same; Genus_T, Genus_Y }\n    Family_T [label = \"Does f (the Family of g)\nhave >=10 records\nfor the term t and unit u?\"]\n    { rank = same; Family_T, Family_Y }\n    Order_T [label = \"Does o (the Order of f)\nhave >=10 records\nfor the term t and unit u?\"]\n    { rank = same; Order_T, Order_Y }\n    Class_T [label = \"Does c (the Class of o)\nhave any record\nfor the term t and unit u?\"]\n    { rank = same; Class_T, Class_Y }\n\n    # Yes edges\n    Inp -> Species_T\n    edge [arrowhead = \"box\", label = \"Yes\"]\n    Species_T -> Species_Y\n    Genus_T -> Genus_Y\n    Family_T -> Family_Y\n    Order_T -> Order_Y\n    Class_T -> Class_Y\n    \n    # No edges\n    edge [label = \"     No\"]\n    Species_T -> Genus_T\n    Genus_T -> Family_T\n    Family_T -> Order_T\n    Order_T -> Class_T\n    Class_T -> Kingdom_Y\n\n  }\n","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

To sum up the flowchart above, we will first count the number of records
under the given species, term and unit. If the records are insufficient,
we then “roll-up” and attempt to find the number of records for the
genus associated with the species. This process continues until we have
sufficient data points to calculate the median.

##### Managing Outliers

Another potential challenge of converting around the median is that
outliers may be extremely high, thereby skewing the aggregate results
towards certain records. To remedy this, we will floor the scaled
quantity (\(q_{nt} / m_{nt}\)) of each record to the 5th percentile and
cap it to the 95th percentile.

Using the following process, we can now convert all the terms to their
respective standardized units.

``` r
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
                          (granular != 'Kingdom' & Records < 10 & !is.na(ProposedRecord)),
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

dataset <- convertViaMedian(dataset, target_unit %>% select(Term, Unit))

post_clean_str()
```

    ## 71 term-unit pairs remaining (68% of total removed)

#### Well-Defined Terms

Let us now <span class="hl">convert each term to the universal animal
unit</span>. Through observation, the terms can be differentiated into
well-defined or ambiguous. A term is well-defined if it is consistent
numerically across all animals and records. One good example would be
tusk, since 2 of them are always the equivalent of 1 animal.

Listed below are the terms we describe as
well-defined:

``` r
well_defined_terms <- read.csv(paste0(specs_dir, "well_defined_terms.csv"))

output_str <- "Well-Defined Terms [with Number Per Live Animal]:\n"
wdt <- well_defined_terms %>%
       mutate(outputString = paste0(Term," [", perAnimal, "]"))
output_str <- output_str %>%
              paste0(paste0(wdt$outputString,collapse=", "))
  
cat(output_str)
```

    ## Well-Defined Terms [with Number Per Live Animal]:
    ## live [1], trophies [1], raw corals [1], tusks [2], skulls [1], bodies [1], tails [1], ears [2], genitalia [1], horns [1], trunk [1]

To convert a well-defined term, we simply need to divide the quantity by
the number of terms per animal:

``` r
dataset <- dataset %>%
  left_join(well_defined_terms, by="Term") %>%
  mutate(isWellDefined = !is.na(perAnimal),
         Term = ifelse(isWellDefined, "animal",Term),
         Unit = ifelse(isWellDefined, "", Unit),
         Qty = ifelse(isWellDefined, 1. / perAnimal, 1.) * Qty) %>%
  select(-perAnimal, -isWellDefined)

post_clean_str()
```

    ## 61 term-unit pairs remaining (72% of total removed)

#### Ambiguous Terms

The final piece of the jigsaw is to convert ambiguous terms. These
terms, by nature of their names, are somewhat vague, and there exists no
clear association between them and the number of animals. For example,
how many ivory pieces make up an elephant? Depending on the sizes of the
pieces, the numbers would differ record by record.

To simplify the conversion process, we will revisit our previous
assumption when standardizing units of a term. Assuming that
<span class="hl">across each species, the median quantity traded in live
animal units is the same across any term</span>, ambiguous terms can be
converted using the following equation:

\[ q_{animal} = \frac{q_{at}}{m_{at}} \times m_{animal} \]

where <br> \(q_{animal}\) corresponds to the quantity of animals traded,
<br> \(q_{at}\) corresponds to the quantity traded (in ambiguous terms),
<br> \(m_{at}\) corresponds to the median quantity traded (in ambiguous
terms), and <br> \(m_{animal}\) corresponds to the median quantity of
animals traded.

As before, medians are obtained through the
<a href="#the-roll-up-median-approach">roll-up approach</a>, and
outliers are managed by <a href="#managing-outliers">flooring and
capping at the 5th and 95th percentile</a> respectively.

With this final step, we have reduced over 200 distinct term-unit pairs
into a single animal unit\!

``` r
target <- data_frame(Term = "animal")
dataset <- convertViaMedian(dataset, target)

post_clean_str()
```

    ## 1 term-unit pair remaining (99% of total removed)

## Exploration

## Tracing Supply and Demand

### The Algorithm

### Following the Demand

### Finding the Supply

## Summary of Results

## Limitations

  - Wild
  - Unidentified Species

## References
