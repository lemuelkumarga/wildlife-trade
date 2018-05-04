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
packages <- c("dplyr","ggplot2","tidyr","pander","DiagrammeR","htmlwidgets","streamgraph","sunburstR")
load_or_install.packages(packages)

data_dir <- "data/"
specs_dir <- "specs/"

si <- sessionInfo()
base_pkg_str <- paste0("Base Packages: ",paste(si[["basePkgs"]], collapse=", "))
attached_pkg_str <- paste0("Attached Packages: ",paste(names(si[["otherPkgs"]]), collapse=", "))
cat(paste0(base_pkg_str,"\n",attached_pkg_str))
```

    ## Base Packages: stats, graphics, grDevices, utils, datasets, methods, base
    ## Attached Packages: bindrcpp, sunburstR, streamgraph, htmlwidgets, DiagrammeR, tidyr, pander, ggplot2, dplyr, knitr

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

<div id="htmlwidget-ad281581f96f498da334" class="grViz html-widget" style="width:672px;height:700px;">

</div>

<script type="application/json" data-for="htmlwidget-ad281581f96f498da334">{"x":{"diagram":"\n  digraph RollUp {\n\n    # Default Specs\n    graph [compound = true, nodesep = .5, ranksep = .25]\n    node [fontname = \"Source Sans Pro\", fontsize = 14, fontcolor = \"#ffffff\", penwidth=0, color=\"#424242BF\", style=filled]\n    edge [fontname = \"Source Sans Pro\", fontcolor = \"#424242BF\", color=\"#424242BF\"]\n    \n    # Input Specs\n    Inp [fillcolor = \"#424242BF\", label = \"(Species s, Term t, Unit u)\", shape = rectangle]\n\n    # Conclude Specs\n    node [shape = oval]\n    Species_Y [label = \"Use the median quantity\nof Species s, Term t and Unit u.\", fillcolor = \"#335A87\"]\n    Genus_Y [label = \"Use the median quantity\nof Genus g, Term t and Unit u.\", fillcolor = \"#3C7759\"]\n    Family_Y [label = \"Use the median quantity\nof Family f, Term t and Unit u.\", fillcolor = \"#A43820\"]\n    Order_Y [label = \"Use the median quantity\nof Order o, Term t and Unit u.\", fillcolor = \"#5C3C7C\"]\n    Class_Y [label = \"Use the median quantity\nof Class c, Term t and Unit u.\", fillcolor = \"#377D95\"]\n    Kingdom_Y [label = \"Use the median quantity\nof Term t and Unit u (across all).\", fillcolor = \"#424242\"]\n\n    # Trigger Specs\n    node [shape = diamond, fillcolor = \"#ffffff\", fontcolor = \"#424242\", penwidth = 1]\n    Species_T [label = \"Does the Species s\nhave >=10 records\nfor the term t and unit u?\"]\n    { rank = same; Species_T, Species_Y }\n    Genus_T [label = \"Does g (the Genus of s)\nhave >=10 records\nfor the term t and unit u?\"]\n    { rank = same; Genus_T, Genus_Y }\n    Family_T [label = \"Does f (the Family of g)\nhave >=10 records\nfor the term t and unit u?\"]\n    { rank = same; Family_T, Family_Y }\n    Order_T [label = \"Does o (the Order of f)\nhave >=10 records\nfor the term t and unit u?\"]\n    { rank = same; Order_T, Order_Y }\n    Class_T [label = \"Does c (the Class of o)\nhave any record\nfor the term t and unit u?\"]\n    { rank = same; Class_T, Class_Y }\n\n    # Yes edges\n    Inp -> Species_T\n    edge [arrowhead = \"box\", label = \"Yes\"]\n    Species_T -> Species_Y\n    Genus_T -> Genus_Y\n    Family_T -> Family_Y\n    Order_T -> Order_Y\n    Class_T -> Class_Y\n    \n    # No edges\n    edge [label = \"     No\"]\n    Species_T -> Genus_T\n    Genus_T -> Family_T\n    Family_T -> Order_T\n    Order_T -> Class_T\n    Class_T -> Kingdom_Y\n\n  }\n","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

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

``` r
trades_by_time <- dataset %>%
                  filter(!(IUCNStatus %in% c("EW","EX"))) %>%
                  mutate(IUCNLabel = ifelse(IUCNStatus == "CR", "Critical",
                                            ifelse(IUCNStatus == "EN", "Endangered",
                                                   "Vulnerable"))) %>%
                  group_by(Year,IUCNLabel) %>%
                  summarise(total_trades = round(sum(Qty)))
                  
trades_by_time$IUCNLabel <- factor(trades_by_time$IUCNLabel, 
                                    levels=c("Vulnerable","Endangered","Critical"),
                                    ordered = TRUE)

streamgraph_plot <- suppressWarnings(
                      streamgraph(trades_by_time, key="IUCNLabel", value="total_trades", date="Year", offset="zero",
                                                   width=750, height=300,left=70) %>%
                      sg_colors(axis_color = ltxt_color, tooltip_color = ltxt_color) %>%
                      sg_axis_x(2) %>%
                      sg_fill_manual(c(get_color("red"), get_color("red", 0.6), get_color("red", 0.3)))
                    )

streamgraph_plot
```

<!--html_preserve-->

<div id="htmlwidget-be1b0092ad990e4ba80d" class="streamgraph html-widget" style="width:750px;height:300px;">

</div>

<div id="htmlwidget-be1b0092ad990e4ba80d-legend" class="streamgraph html-widget-legend" style="width:750">

<center>

<label style='padding-right:5px' for='htmlwidget-be1b0092ad990e4ba80d-select'></label><select id='htmlwidget-be1b0092ad990e4ba80d-select' style='visibility:hidden;'></select>

</center>

</div>

<script type="application/json" data-for="htmlwidget-be1b0092ad990e4ba80d">{"x":{"data":{"key":["Vulnerable","Endangered","Critical","Vulnerable","Endangered","Critical","Vulnerable","Endangered","Critical","Vulnerable","Endangered","Critical","Vulnerable","Endangered","Critical","Vulnerable","Endangered","Critical","Vulnerable","Endangered","Critical","Vulnerable","Endangered","Critical","Vulnerable","Endangered","Critical","Vulnerable","Endangered","Critical","Vulnerable","Endangered","Critical","Vulnerable","Endangered","Critical","Vulnerable","Endangered","Critical","Vulnerable","Endangered","Critical","Vulnerable","Endangered","Critical"],"value":[839477,209092,1746024,631423,143614,2541266,686183,172237,1618047,912981,2773630,1772231,743484,224270,1727029,630264,174758,1910690,633095,1508010,2332082,636721,680345,1995153,683252,346027,2307082,875380,503895,5348063,769794,407877,2339398,896128,534459,3570403,1256355,609341,3041928,1015088,488128,3072103,672331,888918,1816712],"date":["2001-01-01","2001-01-01","2001-01-01","2002-01-01","2002-01-01","2002-01-01","2003-01-01","2003-01-01","2003-01-01","2004-01-01","2004-01-01","2004-01-01","2005-01-01","2005-01-01","2005-01-01","2006-01-01","2006-01-01","2006-01-01","2007-01-01","2007-01-01","2007-01-01","2008-01-01","2008-01-01","2008-01-01","2009-01-01","2009-01-01","2009-01-01","2010-01-01","2010-01-01","2010-01-01","2011-01-01","2011-01-01","2011-01-01","2012-01-01","2012-01-01","2012-01-01","2013-01-01","2013-01-01","2013-01-01","2014-01-01","2014-01-01","2014-01-01","2015-01-01","2015-01-01","2015-01-01"]},"markers":null,"annotations":null,"offset":"zero","interactive":true,"interpolate":"cardinal","palette":["#A43820","#C88779","#E3C3BC"],"text":"#424242BF","tooltip":"#424242BF","x_tick_interval":2,"x_tick_units":"year","x_tick_format":"%Y","y_tick_count":5,"y_tick_format":",g","top":20,"right":40,"bottom":30,"left":70,"legend":false,"legend_label":"","fill":"manual","label_col":"black","x_scale":"date"},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

``` r
# Kudos to http://timelyportfolio.github.io/sunburstR/example_baseball.html
# for sunburst reference
trades_by_species <- dataset %>%
                     group_by(Class, Order, Family, Genus, Taxon, CommonName) %>%
                     summarise(total_trades = sum(Qty)) %>%
                     ungroup()
trades_by_species[['CommonName']] <- sapply(trades_by_species[['CommonName']], function(x) { strsplit(x,", ")[[1]][1] })

sunburst_input <- trades_by_species %>%
                  group_by(Class) %>%
                  mutate(Node = ifelse(grepl("spp.",Taxon), Class,ifelse(is.na(CommonName),Taxon, gsub("-"," ",CommonName))),
                         Category = Class,
                         CategorySize = sum(total_trades),
                         Seq = ifelse(grepl("spp.",Taxon),
                                      Node,
                                      paste(Class,Node, sep="-")),
                         Depth = ifelse(grepl("spp.",Taxon),
                                        1,
                                        2)) %>%
                  ungroup() %>% group_by(Node, Category, CategorySize, Seq, Depth) %>%
                  summarise(Value = sum(total_trades)) %>%
                  ungroup() %>%
                  # Remove any categories than 0.01 percent since they won't appear anyway
                  filter(CategorySize >= 0.0001 * sum(Value))

# Add Categories That Are Not Represented By A Row
no_node_categories <- unique(sunburst_input$Category[!(sunburst_input$Category %in% sunburst_input$Node)])
additional_nodes <- sunburst_input %>%
                    filter(sunburst_input$Category %in% no_node_categories) %>%
                    mutate(Node = Category,
                           Seq = Category,
                           Depth = 1,
                           Value = 0) %>%
                    unique()

sunburst_input <- rbind(sunburst_input,
                        additional_nodes) %>%
                  arrange(Depth, desc(CategorySize), desc(Value))

# Set the colors for each node
sunburst_palette <- get_color("palette")(length(unique(sunburst_input$Category)))
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

# Centerize the sunburst plot
htmlwidgets::onRender(
    sunburst_plot,
    "
    function(el, x) {
    $('.sunburst-chart').css('width', 'initial');
    $('.sunburst-chart').css('height', 'initial');
    $('.sunburst-chart').css('left', '50%');
    $('.sunburst-chart').css('transform', 'translate(-50%, 0)');
    }
    "
)
```

<!--html_preserve-->

<div id="htmlwidget-f0042b29538122bd44db" class="sunburst html-widget" style="width:600px;height:384px; position:relative;">

<div>

<div class="sunburst-main">

<div class="sunburst-sequence">

</div>

<div class="sunburst-chart">

<div class="sunburst-explanation" style="visibility:hidden;">

</div>

</div>

</div>

<div class="sunburst-sidebar">

<input type="checkbox" class="sunburst-togglelegend" style="visibility:hidden;">Legend</input>

<div class="sunburst-legend" style="visibility:hidden;">

</div>

</div>

</div>

</div>

<script type="application/json" data-for="htmlwidget-f0042b29538122bd44db">{"x":{"data":{"children":[{"name":"Anthozoa","children":[{"name":"Heliofungia actiniformis","size":840416.1841,"colname":"X2"},{"name":"Catalaphyllia jardinei","size":721570.9776,"colname":"X2"},{"name":"Euphyllia ancora","size":644428.4883,"colname":"X2"},{"name":"Euphyllia cristata","size":546594.2983,"colname":"X2"},{"name":"Turbinaria peltata","size":256861.8016,"colname":"X2"},{"name":"Turbinaria mesenterina","size":203756.1909,"colname":"X2"},{"name":"Physogyra lichtensteini","size":197090.2562,"colname":"X2"},{"name":"Blue Coral","size":142687.2031,"colname":"X2"},{"name":"Galaxea astreata","size":115712.2868,"colname":"X2"},{"name":"Turbinaria reniformis","size":51298.2068,"colname":"X2"},{"name":"Euphyllia paraancora","size":50532,"colname":"X2"},{"name":"Porites nigrescens","size":50219,"colname":"X2"},{"name":"Elkhorn Coral","size":49237.151,"colname":"X2"},{"name":"Pachyseris rugosa","size":48057.9852,"colname":"X2"},{"name":"Euphyllia paradivisa","size":45865,"colname":"X2"},{"name":"Moseleya latistellata","size":43146.4,"colname":"X2"},{"name":"Lettuce Coral","size":29154.7067,"colname":"X2"},{"name":"Staghorn Coral","size":19395.8882,"colname":"X2"},{"name":"Acropora echinata","size":18121.3333,"colname":"X2"},{"name":"Acropora horrida","size":15134.7616,"colname":"X2"},{"name":"Acropora aspera","size":10853.5704,"colname":"X2"},{"name":"Acanthastrea bowerbanki","size":8556,"colname":"X2"},{"name":"Acropora microclados","size":6224.6142,"colname":"X2"},{"name":"Montipora capricornis","size":6006,"colname":"X2"},{"name":"Cactus Coral","size":5388.8,"colname":"X2"},{"name":"Alveopora gigas","size":5138,"colname":"X2"},{"name":"Pavona cactus","size":5110.4129,"colname":"X2"},{"name":"Acropora vaughani","size":4801,"colname":"X2"},{"name":"Acropora pharaonis","size":3449.5763,"colname":"X2"},{"name":"Acropora abrolhosensis","size":3395.7835,"colname":"X2"},{"name":"Acropora aculeus","size":3252,"colname":"X2"},{"name":"Acropora paniculata","size":3040,"colname":"X2"},{"name":"Acropora anthocercis","size":2708.6142,"colname":"X2"},{"name":"Acropora caroliniana","size":2582,"colname":"X2"},{"name":"Acropora jacquelineae","size":2445,"colname":"X2"},{"name":"Australogyra zelli","size":2292,"colname":"X2"},{"name":"Acropora palmerae","size":2289.5443,"colname":"X2"},{"name":"Acropora kimbeensis","size":1820,"colname":"X2"},{"name":"Acropora hemprichii","size":1749.6142,"colname":"X2"},{"name":"Acropora polystoma","size":1674.0158,"colname":"X2"},{"name":"Acropora suharsonoi","size":1657,"colname":"X2"},{"name":"Acropora papillare","size":1560,"colname":"X2"},{"name":"Acropora hoeksemai","size":1463,"colname":"X2"},{"name":"Cyphastrea ocellina","size":1276,"colname":"X2"},{"name":"Acropora desalwii","size":1084,"colname":"X2"},{"name":"Acropora donei","size":1030.3401,"colname":"X2"},{"name":"Acropora batunai","size":971,"colname":"X2"},{"name":"Porites horizontalata","size":947,"colname":"X2"},{"name":"Acropora loisetteae","size":807,"colname":"X2"},{"name":"Acropora verweyi","size":775.0682,"colname":"X2"},{"name":"Pocillopora elegans","size":729.0857,"colname":"X2"},{"name":"Acropora multiacuta","size":712.132,"colname":"X2"},{"name":"Acanthastrea hemprichii","size":619,"colname":"X2"},{"name":"Fungia curvata","size":610,"colname":"X2"},{"name":"Psammocora stellata","size":581.3218,"colname":"X2"},{"name":"Acropora listeri","size":546,"colname":"X2"},{"name":"Elliptical Star Coral","size":485.6719,"colname":"X2"},{"name":"Pocillopora danae","size":451.9966,"colname":"X2"},{"name":"Acropora speciosa","size":414,"colname":"X2"},{"name":"Lobophyllia diminuta","size":404,"colname":"X2"},{"name":"Acropora turaki","size":378,"colname":"X2"},{"name":"Turbinaria heronensis","size":374,"colname":"X2"},{"name":"Acropora simplex","size":348.4246,"colname":"X2"},{"name":"Montipora caliculata","size":302.5714,"colname":"X2"},{"name":"Plerogyra discus","size":285,"colname":"X2"},{"name":"Pectinia alcicornis","size":253.1538,"colname":"X2"},{"name":"Large Ivory Coral","size":232,"colname":"X2"},{"name":"Acropora solitaryensis","size":216,"colname":"X2"},{"name":"Montipora calcarea","size":199.2857,"colname":"X2"},{"name":"Leptoria irregularis","size":184,"colname":"X2"},{"name":"Acropora willisae","size":183.5698,"colname":"X2"},{"name":"Acropora kosurini","size":179,"colname":"X2"},{"name":"Leptoseris yabei","size":178,"colname":"X2"},{"name":"Pavona venosa","size":163.2,"colname":"X2"},{"name":"Alveopora excelsa","size":150,"colname":"X2"},{"name":"Acropora dendrum","size":131,"colname":"X2"},{"name":"Mediterranean Pillow Coral","size":121,"colname":"X2"},{"name":"Rough Cactus Coral","size":119,"colname":"X2"},{"name":"Pillar Coral","size":117.2279,"colname":"X2"},{"name":"Acropora lokani","size":103,"colname":"X2"},{"name":"Porites attenuata","size":95,"colname":"X2"},{"name":"Turbinaria stellulata","size":92,"colname":"X2"},{"name":"Acropora acuminata","size":85.3205,"colname":"X2"},{"name":"Acropora lovelli","size":85,"colname":"X2"},{"name":"Goniopora albiconus","size":78,"colname":"X2"},{"name":"Acropora globiceps","size":76,"colname":"X2"},{"name":"Acropora walindii","size":69,"colname":"X2"},{"name":"Acropora awi","size":62,"colname":"X2"},{"name":"Montipora angulata","size":59.2857,"colname":"X2"},{"name":"Acropora roseni","size":55,"colname":"X2"},{"name":"Acropora elegans","size":51,"colname":"X2"},{"name":"Turbinaria bifrons","size":46,"colname":"X2"},{"name":"Montipora samarensis","size":44,"colname":"X2"},{"name":"Lamarck's Sheet Coral","size":42.5,"colname":"X2"},{"name":"Turbinaria patula","size":40,"colname":"X2"},{"name":"Anomastraea irregularis","size":39,"colname":"X2"},{"name":"Montipora australiensis","size":34.7143,"colname":"X2"},{"name":"Echinophyllia costata","size":34,"colname":"X2"},{"name":"Polycyathus isabela","size":33,"colname":"X2"},{"name":"Acropora striata","size":32.1037,"colname":"X2"},{"name":"Alveopora verrilliana","size":32,"colname":"X2"},{"name":"Alveopora allingi","size":30,"colname":"X2"},{"name":"Montipora cocosensis","size":23,"colname":"X2"},{"name":"Montipora stilosa","size":23,"colname":"X2"},{"name":"Leptoseris incrustans","size":20.8402,"colname":"X2"},{"name":"Acanthastrea faviaformis","size":20,"colname":"X2"},{"name":"Acanthastrea ishigakiensis","size":20,"colname":"X2"},{"name":"Pavona bipartita","size":20,"colname":"X2"},{"name":"Montipora cebuensis","size":18,"colname":"X2"},{"name":"Symphyllia hassi","size":17,"colname":"X2"},{"name":"Floreana Coral","size":14,"colname":"X2"},{"name":"Acropora retusa","size":12.1518,"colname":"X2"},{"name":"Acropora tenella","size":12,"colname":"X2"},{"name":"Anacropora spinosa","size":11,"colname":"X2"},{"name":"Pavona danai","size":10.4,"colname":"X2"},{"name":"Echinopora ashmorensis","size":10,"colname":"X2"},{"name":"Hydnophora bonsai","size":10,"colname":"X2"},{"name":"Porites cumulatus","size":10,"colname":"X2"},{"name":"Favites spinosa","size":9.4737,"colname":"X2"},{"name":"Acropora russelli","size":9,"colname":"X2"},{"name":"Galaxea acrhelia","size":8,"colname":"X2"},{"name":"Leptastrea aequalis","size":7,"colname":"X2"},{"name":"Lobophyllia dentatus","size":6,"colname":"X2"},{"name":"Montipora patula","size":6,"colname":"X2"},{"name":"Mycedium steeni","size":6,"colname":"X2"},{"name":"Acropora kirstyae","size":5,"colname":"X2"},{"name":"Montastrea salebrosa","size":5,"colname":"X2"},{"name":"Acropora spicifera","size":4,"colname":"X2"},{"name":"Astreopora incrustans","size":4,"colname":"X2"},{"name":"Siderastrea glynni","size":4,"colname":"X2"},{"name":"Lobophyllia flabelliformis","size":3,"colname":"X2"},{"name":"Montipora altasepta","size":3,"colname":"X2"},{"name":"Acanthastrea regularis","size":2,"colname":"X2"},{"name":"Acropora derawanensis","size":2,"colname":"X2"},{"name":"Acropora indonesia","size":2,"colname":"X2"},{"name":"Ctenella chagius","size":2,"colname":"X2"},{"name":"Montastrea multipunctata","size":2,"colname":"X2"},{"name":"Montipora crassituberculata","size":2,"colname":"X2"},{"name":"Montipora setosa","size":2,"colname":"X2"},{"name":"Astreopora cucullata","size":1.6667,"colname":"X2"},{"name":"Fungia taiwanensis","size":1,"colname":"X2"},{"name":"Platygyra yaeyamaensis","size":1,"colname":"X2"},{"name":"Porites aranetai","size":1,"colname":"X2"},{"name":"Porites okinawensis","size":1,"colname":"X2"}],"size":36277870.0825,"colname":"X1"},{"name":"Actinopteri","children":[{"name":"European Eel","size":5641889.6212,"colname":"X2"},{"name":"Giant Seahorse","size":308810.3505,"colname":"X2"},{"name":"Spotted Seahorse","size":209118.0004,"colname":"X2"},{"name":"Banggai Cardinalfish","size":118431.9289,"colname":"X2"},{"name":"Giant Wrasse","size":87452.0185,"colname":"X2"},{"name":"Stellate Sturgeon","size":85473.5336,"colname":"X2"},{"name":"Beluga","size":82154.1098,"colname":"X2"},{"name":"Persian Sturgeon","size":65546.904,"colname":"X2"},{"name":"Russian Sturgeon","size":65353.9537,"colname":"X2"},{"name":"Three spot Seahorse","size":62655.3676,"colname":"X2"},{"name":"Hedgehog Seahorse","size":42022.5703,"colname":"X2"},{"name":"Great Seahorse","size":40698.832,"colname":"X2"},{"name":"Thorny Seahorse","size":35233.3774,"colname":"X2"},{"name":"Asian Arowana","size":31559.7307,"colname":"X2"},{"name":"Lined Seahorse","size":23923.3145,"colname":"X2"},{"name":"Barbour's Seahorse","size":20325.088,"colname":"X2"},{"name":"Tiger Tail Seahorse","size":18939.4292,"colname":"X2"},{"name":"Paddlefish","size":16175.9111,"colname":"X2"},{"name":"West African Seahorse","size":13322.9831,"colname":"X2"},{"name":"Siberian Sturgeon","size":5692.2933,"colname":"X2"},{"name":"Sterlet","size":4770.2523,"colname":"X2"},{"name":"Kaluga","size":3314.9112,"colname":"X2"},{"name":"Amur Sturgeon","size":1894.7941,"colname":"X2"},{"name":"White's Seahorse","size":1202.2679,"colname":"X2"},{"name":"Shovelnose Sturgeon","size":1123.1822,"colname":"X2"},{"name":"Ship Sturgeon","size":675.2994,"colname":"X2"},{"name":"False Shovelnose Sturgeon","size":129.1053,"colname":"X2"},{"name":"Japanese Seahorse","size":92.081,"colname":"X2"},{"name":"Atlantic Sturgeon","size":71.9787,"colname":"X2"},{"name":"Dwarf Sturgeon","size":61.9737,"colname":"X2"},{"name":"Congo Blind Barb","size":45,"colname":"X2"},{"name":"Pallid Sturgeon","size":7.5,"colname":"X2"},{"name":"Shortnose Sturgeon","size":2.3641,"colname":"X2"},{"name":"Totoaba","size":1.5314,"colname":"X2"}],"size":75958.4881,"colname":"X1"},{"name":"Holothuroidea","children":[{"name":"Brown Sea Cucumber","size":6394609.3386,"colname":"X2"}],"size":0,"colname":"X1"},{"name":"Reptilia","children":[{"name":"Afghan Tortoise","size":718457.4482,"colname":"X2"},{"name":"Southeast Asian Box Turtle","size":700335.4856,"colname":"X2"},{"name":"Asiatic Softshell Turtle","size":400235.2333,"colname":"X2"},{"name":"Alligator Snapping Turtle","size":341817,"colname":"X2"},{"name":"Chinese Stripe necked Turtle","size":110985.4808,"colname":"X2"},{"name":"Black Marsh Turtle","size":105401.3727,"colname":"X2"},{"name":"Giant Asian Pond Turtle","size":93755.9487,"colname":"X2"},{"name":"Burmese Python","size":89703.0892,"colname":"X2"},{"name":"Common Tortoise","size":78629.4509,"colname":"X2"},{"name":"Yellow headed Temple Turtle","size":68341.6667,"colname":"X2"},{"name":"Reeves' Turtle","size":66330.799,"colname":"X2"},{"name":"Malaysian Giant Turtle","size":52382.4615,"colname":"X2"},{"name":"Spiny Turtle","size":33634.7692,"colname":"X2"},{"name":"Indian Star Tortoise","size":32969.1449,"colname":"X2"},{"name":"Siamese Crocodile","size":22312.1728,"colname":"X2"},{"name":"Vietnamese Pond Turtle","size":20542,"colname":"X2"},{"name":"Home's Hinge back Tortoise","size":20381.3052,"colname":"X2"},{"name":"Uroplatus ebenaui","size":18479.5627,"colname":"X2"},{"name":"Four horned Chameleon","size":12629,"colname":"X2"},{"name":"Crested Gecko","size":10633,"colname":"X2"},{"name":"Malayan Flat shelled Turtle","size":8483,"colname":"X2"},{"name":"Egyptian Mastigure","size":7766.7586,"colname":"X2"},{"name":"Painted Terrapin","size":7594.926,"colname":"X2"},{"name":"Yellow Pond Turtle","size":6756,"colname":"X2"},{"name":"Green Turtle","size":5964.1437,"colname":"X2"},{"name":"Kemp's Ridley","size":5087.557,"colname":"X2"},{"name":"Elongated Tortoise","size":4840.8104,"colname":"X2"},{"name":"Crevice Tortoise","size":4731,"colname":"X2"},{"name":"Uroplatus henkeli","size":4481.5631,"colname":"X2"},{"name":"Celebes Tortoise","size":4362.6892,"colname":"X2"},{"name":"Asian Giant Tortoise","size":4162.4242,"colname":"X2"},{"name":"Olive Ridley","size":4000.4507,"colname":"X2"},{"name":"King Cobra","size":3967.1456,"colname":"X2"},{"name":"Chinese Softshell Turtle","size":3942,"colname":"X2"},{"name":"Hawksbill Turtle","size":3488.3554,"colname":"X2"},{"name":"Spider Tortoise","size":3457.1184,"colname":"X2"},{"name":"African Spurred Tortoise","size":3057.25,"colname":"X2"},{"name":"Yellow spotted River Turtle","size":2754.4553,"colname":"X2"},{"name":"Keeled Box Turtle","size":2535.06,"colname":"X2"},{"name":"Chinese Red necked Turtle","size":2304.8161,"colname":"X2"},{"name":"Mekong Snail eating Turtle","size":2270.7949,"colname":"X2"},{"name":"Uroplatus pietschmanni","size":2150,"colname":"X2"},{"name":"Decary's Leaf Chameleon","size":1990.7071,"colname":"X2"},{"name":"Ploughshare Tortoise","size":1726.9611,"colname":"X2"},{"name":"Palawan Forest Turtle","size":1670,"colname":"X2"},{"name":"Flat tailed Tortoise","size":1519.3323,"colname":"X2"},{"name":"Loggerhead Turtle","size":1492.4515,"colname":"X2"},{"name":"Central American River Turtle","size":1463.1429,"colname":"X2"},{"name":"Black breasted Leaf Turtle","size":1453,"colname":"X2"},{"name":"Leatherback","size":1412.3359,"colname":"X2"},{"name":"Uroplatus guentheri","size":1141.3373,"colname":"X2"},{"name":"Lesser Antillean Green Iguana","size":1083,"colname":"X2"},{"name":"Four lined Girdled Lizard","size":1011.8966,"colname":"X2"},{"name":"Laborde's Chameleon","size":980.4355,"colname":"X2"},{"name":"New Guinea Giant Softshell Turtle","size":968.4274,"colname":"X2"},{"name":"Madagascar Big headed Turtle","size":941.2821,"colname":"X2"},{"name":"Furcifer campani","size":853,"colname":"X2"},{"name":"Brookesia minima","size":829.5,"colname":"X2"},{"name":"Island Day Gecko","size":824.5,"colname":"X2"},{"name":"Ringed Sawback","size":800,"colname":"X2"},{"name":"Radiated Tortoise","size":768.5931,"colname":"X2"},{"name":"Meadow Viper","size":735.5968,"colname":"X2"},{"name":"Furcifer rhinoceratus","size":731.9327,"colname":"X2"},{"name":"Tiger Chameleon","size":720.8333,"colname":"X2"},{"name":"Asian Giant Softshell Turtle","size":682.4274,"colname":"X2"},{"name":"Eiongate Leaf Chameleon","size":633.1,"colname":"X2"},{"name":"Bakossi Two horned Chameleon","size":579.5833,"colname":"X2"},{"name":"O'Shaughnessy's Chameleon","size":513.4933,"colname":"X2"},{"name":"Sulawesi Forest Turtle","size":498,"colname":"X2"},{"name":"Yellow margined Box Turtle","size":487.8395,"colname":"X2"},{"name":"Chaco Tortoise","size":431.8621,"colname":"X2"},{"name":"Fiji Crested Iguana","size":428.5241,"colname":"X2"},{"name":"African Softshell Turtle","size":418.2837,"colname":"X2"},{"name":"Indochinese Box Turtle ","size":410.5,"colname":"X2"},{"name":"Chinese Three striped Box Turtle","size":349.9121,"colname":"X2"},{"name":"Uroplatus malama","size":336.5,"colname":"X2"},{"name":"Coahuila Box Turtle","size":334.8996,"colname":"X2"},{"name":"Round Island Day Gecko","size":324.5977,"colname":"X2"},{"name":"Northern Leaf Chameleon","size":323,"colname":"X2"},{"name":"Antimena Chameleon","size":311.3143,"colname":"X2"},{"name":"Impressed Tortoise","size":288.4246,"colname":"X2"},{"name":"Red headed Amazon River Turtle","size":279.2036,"colname":"X2"},{"name":"Petter's Chameleon","size":269.8183,"colname":"X2"},{"name":"Furcifer nicosiai","size":268.3483,"colname":"X2"},{"name":"Wood Turtle","size":230.4,"colname":"X2"},{"name":"Natal Midlands Dwarf Chameleon","size":228.1905,"colname":"X2"},{"name":"American Crocodile","size":218.375,"colname":"X2"},{"name":"Pig nosed Turtle","size":216.4667,"colname":"X2"},{"name":"Phelsuma antanosy","size":194.9167,"colname":"X2"},{"name":"Asian Narrow headed Softshell Turtle","size":185,"colname":"X2"},{"name":"Mojave Desert Tortoise","size":183.5524,"colname":"X2"},{"name":"Fernandina Marine Iguana","size":179.3889,"colname":"X2"},{"name":"Big headed Amazon River Turtle","size":179.131,"colname":"X2"},{"name":"Round Island Keel scaled Boa","size":161.3689,"colname":"X2"},{"name":"Burmese Starred Tortoise","size":156,"colname":"X2"},{"name":"Northern Bahamian Rock Iguana","size":155.813,"colname":"X2"},{"name":"Big headed Turtle","size":148.8962,"colname":"X2"},{"name":"Antsingy Leaf Chameleon","size":147.8399,"colname":"X2"},{"name":"Egyptian Tortoise","size":139,"colname":"X2"},{"name":"Calumma furcifer","size":135,"colname":"X2"},{"name":"Eastern Box Turtle","size":131.5,"colname":"X2"},{"name":"Calumma globifer","size":124.3763,"colname":"X2"},{"name":"Brookesia peyrierasi","size":118,"colname":"X2"},{"name":"Durban Dwarf Chameleon","size":117.1905,"colname":"X2"},{"name":"African Dwarf Crocodile","size":109.9867,"colname":"X2"},{"name":"Phelsuma serraticauda","size":107.1667,"colname":"X2"},{"name":"Mount d'Ambre Leaf Chameleon","size":105,"colname":"X2"},{"name":"Common Land Iguana","size":102.629,"colname":"X2"},{"name":"Mertens' Day Gecko","size":102.1667,"colname":"X2"},{"name":"Yellow headed Box Turtle","size":100,"colname":"X2"},{"name":"Roatán Spiny tailed Iguana","size":97.0456,"colname":"X2"},{"name":"Black chested Spiny tailed Iguana","size":95.0206,"colname":"X2"},{"name":"Brookesia vadoni","size":92,"colname":"X2"},{"name":"Turks and Caicos Rock Iguana","size":91.7016,"colname":"X2"},{"name":"Brookesia karchei","size":91,"colname":"X2"},{"name":"Utila Spiny tailed Iguana","size":89.1456,"colname":"X2"},{"name":"McCord's Box Turtle","size":86,"colname":"X2"},{"name":"Giant Dragon Lizard","size":82.2414,"colname":"X2"},{"name":"Panay Monitor Lizard","size":81.8182,"colname":"X2"},{"name":"Magdalena River Turtle","size":74.2693,"colname":"X2"},{"name":"Chinese Cobra","size":73.1429,"colname":"X2"},{"name":"Floreana Giant Tortoise","size":70.0767,"colname":"X2"},{"name":"Geometric Tortoise","size":67.6271,"colname":"X2"},{"name":"Anegada Rock Iguana","size":67.5588,"colname":"X2"},{"name":"Standing's Day Gecko","size":63,"colname":"X2"},{"name":"Cape Dwarf Chameleon","size":62.8571,"colname":"X2"},{"name":"Burmese Roofed Turtle","size":62.5363,"colname":"X2"},{"name":"Jamaican Iguana","size":58.4791,"colname":"X2"},{"name":"Spiny sided Chameleon","size":57.6667,"colname":"X2"},{"name":"Four eyed Turtle","size":57,"colname":"X2"},{"name":"Usambara Spiny Pygmy Chameleon","size":55,"colname":"X2"},{"name":"Elandsberg Dwarf Chameleon","size":52.8571,"colname":"X2"},{"name":"Pondo Dwarf Chameleon","size":52.8571,"colname":"X2"},{"name":"Clouded Rock Iguana","size":52.4303,"colname":"X2"},{"name":"Northern River Terrapin","size":51.9405,"colname":"X2"},{"name":"Yellow throated Day Gecko","size":51,"colname":"X2"},{"name":"Blanding's Turtle","size":50.1,"colname":"X2"},{"name":"Calumma gallus","size":46,"colname":"X2"},{"name":"Perret's Montane Chameleon","size":45,"colname":"X2"},{"name":"Terrestrial Arboreal Alligator Lizard","size":43,"colname":"X2"},{"name":"Lesser Chameleon","size":42.8,"colname":"X2"},{"name":"Orinoco Crocodile","size":40.7778,"colname":"X2"},{"name":"Six tubercled Amazon River Turtle","size":37.1948,"colname":"X2"},{"name":"Blue Speckled Tree Monitor ","size":36,"colname":"X2"},{"name":"Philippine Crocodile","size":35.1712,"colname":"X2"},{"name":"Grand Cayman Blue Iguana","size":33.203,"colname":"X2"},{"name":"Tsaratanana Chameleon","size":33.2,"colname":"X2"},{"name":"Trioceros serratus","size":33,"colname":"X2"},{"name":"Chinese Crocodile Lizard","size":31.4655,"colname":"X2"},{"name":"Brookesia valerieae","size":30,"colname":"X2"},{"name":"Pan's Box Turtle","size":30,"colname":"X2"},{"name":"Two banded Chameleon","size":29.7055,"colname":"X2"},{"name":"Lau Banded Iguana","size":28.125,"colname":"X2"},{"name":"Komodo Dragon","size":26.8182,"colname":"X2"},{"name":"Phelsuma breviceps","size":24.963,"colname":"X2"},{"name":"Brookesia dentata","size":24.6429,"colname":"X2"},{"name":"Speckled Dwarf Tortoise","size":24,"colname":"X2"},{"name":"Hispaniolan Rhinoceros Iguana","size":22.9182,"colname":"X2"},{"name":"Central Bahamian Rock Iguana","size":22.7537,"colname":"X2"},{"name":"Galápagos Pink Land Iguana","size":22.6059,"colname":"X2"},{"name":"Dwarf Crag Lizard","size":21.5517,"colname":"X2"},{"name":"Usambara Flap nosed Chameleon","size":21,"colname":"X2"},{"name":"Santa Fe Land Iguana","size":19.4625,"colname":"X2"},{"name":"Exiliboa placata","size":17,"colname":"X2"},{"name":"Calumma hilleniusi","size":16.9763,"colname":"X2"},{"name":"Gray's Monitor","size":16.8182,"colname":"X2"},{"name":"Southern River Terrapin","size":15.2568,"colname":"X2"},{"name":"Cuban Crocodile","size":13.8732,"colname":"X2"},{"name":"Assam Roofed Turtle","size":12,"colname":"X2"},{"name":"Brookesia exarmata","size":10.7599,"colname":"X2"},{"name":"Phelsuma seippi","size":10.5,"colname":"X2"},{"name":"Uroplatus malahelo","size":10,"colname":"X2"},{"name":"Vences' Chameleon","size":10,"colname":"X2"},{"name":"Omani Spiny tailed Lizard","size":8,"colname":"X2"},{"name":"Spotted Pond Turtle","size":8,"colname":"X2"},{"name":"False Gharial","size":7.7429,"colname":"X2"},{"name":"Brookesia ramanantsoai","size":7.5,"colname":"X2"},{"name":"Calumma glawi","size":7,"colname":"X2"},{"name":"Phelsuma klemmeri","size":6.3333,"colname":"X2"},{"name":"Arakan Forest Turtle","size":6,"colname":"X2"},{"name":"Phelsuma gigas","size":6,"colname":"X2"},{"name":"Phelsuma hielscheri","size":5.3333,"colname":"X2"},{"name":"Belalanda Chameleon","size":5.1,"colname":"X2"},{"name":"Indian Narrow headed Softshell Turtle","size":5,"colname":"X2"},{"name":"Calumma andringitraense","size":4,"colname":"X2"},{"name":"Calumma peyrierasi","size":4,"colname":"X2"},{"name":"Calumma cucullatum","size":3,"colname":"X2"},{"name":"Uroplatus giganteus","size":3,"colname":"X2"},{"name":"Gopher Tortoise","size":2.2493,"colname":"X2"},{"name":"Calumma capuroni","size":2,"colname":"X2"},{"name":"Pascagoula Map Turtle","size":2,"colname":"X2"},{"name":"Red crowned Roofed Turtle","size":2,"colname":"X2"},{"name":"Python kyaiktiyo","size":1.7455,"colname":"X2"},{"name":"Black And White Spitting Cobra","size":1.5,"colname":"X2"},{"name":"Roti Island Snake necked Turtle","size":1.4246,"colname":"X2"},{"name":"Gharial","size":1.0712,"colname":"X2"},{"name":"Bog Turtle","size":1,"colname":"X2"},{"name":"Phelsuma pronki","size":1,"colname":"X2"},{"name":"Fiji Banded Iguana","size":0.9,"colname":"X2"},{"name":"Mugger","size":0.3597,"colname":"X2"},{"name":"Bolson Tortoise","size":0.2493,"colname":"X2"}],"size":110721.2052,"colname":"X1"},{"name":"Aves","children":[{"name":"Grey Parrot","size":427384.4382,"colname":"X2"},{"name":"Pink Pigeon","size":47841.7234,"colname":"X2"},{"name":"European Turtle dove","size":41345.8821,"colname":"X2"},{"name":"Saker Falcon","size":12877.6729,"colname":"X2"},{"name":"Java Sparrow","size":8916.7207,"colname":"X2"},{"name":"Cape Parrot","size":6497.9786,"colname":"X2"},{"name":"Channel billed Toucan","size":5396.9262,"colname":"X2"},{"name":"Red billed Toucan","size":4117.3231,"colname":"X2"},{"name":"Scheepmaker's Crowned pigeon","size":3616.9267,"colname":"X2"},{"name":"Asian Houbara","size":2521,"colname":"X2"},{"name":"Great Bustard","size":2202.884,"colname":"X2"},{"name":"Spanish Imperial Eagle","size":1698.8996,"colname":"X2"},{"name":"Sun Parakeet","size":1503.5,"colname":"X2"},{"name":"White backed Vulture","size":1399.7383,"colname":"X2"},{"name":"Great Curassow","size":1372.7571,"colname":"X2"},{"name":"Black Crowned crane","size":1315,"colname":"X2"},{"name":"El Oro Parakeet","size":901.3829,"colname":"X2"},{"name":"White Cockatoo","size":749.6429,"colname":"X2"},{"name":"Yellow Cardinal","size":732.7599,"colname":"X2"},{"name":"Mauritius Kestrel","size":677.0202,"colname":"X2"},{"name":"Hooded Vulture","size":604.6449,"colname":"X2"},{"name":"Nicobar Sparrowhawk","size":600,"colname":"X2"},{"name":"African Penguin","size":577.7397,"colname":"X2"},{"name":"African Houbara","size":575.2,"colname":"X2"},{"name":"Abbott's Booby","size":553.6953,"colname":"X2"},{"name":"Humboldt Penguin","size":548.9614,"colname":"X2"},{"name":"Lilac crowned Amazon","size":528.8,"colname":"X2"},{"name":"Yellow naped Amazon","size":454.6212,"colname":"X2"},{"name":"Christmas Frigatebird","size":425.6986,"colname":"X2"},{"name":"White headed Vulture","size":402.88,"colname":"X2"},{"name":"Secretarybird","size":374,"colname":"X2"},{"name":"Crimson bellied Parakeet","size":331.1667,"colname":"X2"},{"name":"Yellow headed Amazon","size":321.3162,"colname":"X2"},{"name":"Grey Crowned crane","size":317,"colname":"X2"},{"name":"Chattering Lory","size":291,"colname":"X2"},{"name":"Yellow crested Cockatoo","size":282.5,"colname":"X2"},{"name":"Red headed Vulture","size":276.0379,"colname":"X2"},{"name":"Rimatara Lorikeet","size":274.2975,"colname":"X2"},{"name":"White rumped Vulture","size":266.4266,"colname":"X2"},{"name":"Egyptian Vulture","size":241.3812,"colname":"X2"},{"name":"Goias Parakeet","size":235.5143,"colname":"X2"},{"name":"Long wattled Umbrellabird","size":216.3333,"colname":"X2"},{"name":"Black Harrier","size":214.9067,"colname":"X2"},{"name":"Banded Cotinga","size":210,"colname":"X2"},{"name":"Lear's Macaw","size":208.0589,"colname":"X2"},{"name":"New Zealand Kaka","size":202.7652,"colname":"X2"},{"name":"Reeves's Pheasant","size":200.5547,"colname":"X2"},{"name":"Oriental Stork","size":188.7052,"colname":"X2"},{"name":"Martial Eagle","size":171.8148,"colname":"X2"},{"name":"Cape Vulture","size":171.5078,"colname":"X2"},{"name":"Swift Parrot","size":170.4,"colname":"X2"},{"name":"Hyacinth Macaw","size":165.3043,"colname":"X2"},{"name":"White necked Rockfowl","size":164.6667,"colname":"X2"},{"name":"Alagoas Curassow","size":153.3333,"colname":"X2"},{"name":"Helmeted Curassow","size":152.5502,"colname":"X2"},{"name":"Seven colored Tanager","size":152,"colname":"X2"},{"name":"Whooping Crane","size":147.4299,"colname":"X2"},{"name":"Visayan Hornbill","size":144.6,"colname":"X2"},{"name":"White eared Parakeet","size":144.3333,"colname":"X2"},{"name":"Kakapo","size":139.7451,"colname":"X2"},{"name":"Chatham Parakeet","size":139.5986,"colname":"X2"},{"name":"Green Peafowl","size":136.0175,"colname":"X2"},{"name":"Steppe Eagle","size":133.2857,"colname":"X2"},{"name":"Kea","size":119.8826,"colname":"X2"},{"name":"Slender billed Vulture","size":115.3864,"colname":"X2"},{"name":"Shoebill","size":113.7143,"colname":"X2"},{"name":"Green thighed Parrot","size":109.3,"colname":"X2"},{"name":"Eastern Imperial Eagle","size":107.5767,"colname":"X2"},{"name":"Goldie's Bird of paradise","size":101.3333,"colname":"X2"},{"name":"Steller's Sea eagle","size":96.4,"colname":"X2"},{"name":"Thick billed Parrot","size":95.2,"colname":"X2"},{"name":"Noisy Scrub bird","size":80.8431,"colname":"X2"},{"name":"Papuan Eagle","size":77.1193,"colname":"X2"},{"name":"Meller's Duck","size":72,"colname":"X2"},{"name":"Bali Myna","size":70,"colname":"X2"},{"name":"Blakiston's Eagle owl","size":69.7993,"colname":"X2"},{"name":"Ludwig's Bustard","size":64,"colname":"X2"},{"name":"White winged Cotinga","size":63,"colname":"X2"},{"name":"Bengal Florican","size":60,"colname":"X2"},{"name":"Great Green Macaw","size":59.1784,"colname":"X2"},{"name":"Salmon crested Cockatoo","size":58.7143,"colname":"X2"},{"name":"Blue Crane","size":56.9099,"colname":"X2"},{"name":"California Condor","size":49.3,"colname":"X2"},{"name":"Northern Rufous Hornbill","size":47.5,"colname":"X2"},{"name":"Southern Black Bustard","size":46,"colname":"X2"},{"name":"Spix's Macaw","size":46,"colname":"X2"},{"name":"St Lucia Amazon","size":42.78,"colname":"X2"},{"name":"Pearly Parakeet","size":41.6667,"colname":"X2"},{"name":"Red crowned Crane","size":41.3581,"colname":"X2"},{"name":"Western Crowned pigeon","size":41.1795,"colname":"X2"},{"name":"Black Curassow","size":41.1576,"colname":"X2"},{"name":"Great Indian Bustard","size":40,"colname":"X2"},{"name":"Lesser Florican","size":40,"colname":"X2"},{"name":"Storm's Stork","size":40,"colname":"X2"},{"name":"Asian Crested Ibis","size":39.0181,"colname":"X2"},{"name":"Pesquet's Parrot","size":38.65,"colname":"X2"},{"name":"Philippine Cockatoo","size":38.2,"colname":"X2"},{"name":"Blue eyed Cockatoo","size":37.5,"colname":"X2"},{"name":"Trinidad Piping guan","size":35.9136,"colname":"X2"},{"name":"Red fronted Macaw","size":35.875,"colname":"X2"},{"name":"Purple naped Lory","size":33,"colname":"X2"},{"name":"Philippine Eagle","size":32.6667,"colname":"X2"},{"name":"Wattled Crane","size":30.5087,"colname":"X2"},{"name":"Palm Lorikeet","size":28,"colname":"X2"},{"name":"Madagascar Teal","size":26.1681,"colname":"X2"},{"name":"Military Macaw","size":25.9881,"colname":"X2"},{"name":"Helmeted Hornbill","size":24.75,"colname":"X2"},{"name":"Malay Crestless Fireback","size":23.6364,"colname":"X2"},{"name":"Malay Peacock pheasant","size":23.6364,"colname":"X2"},{"name":"Blue headed Macaw","size":22,"colname":"X2"},{"name":"Campbell Teal","size":20,"colname":"X2"},{"name":"Palawan Hornbill","size":20,"colname":"X2"},{"name":"White winged Guan","size":20,"colname":"X2"},{"name":"Seychelles Kestrel","size":19.3643,"colname":"X2"},{"name":"Wattled Curassow","size":18.72,"colname":"X2"},{"name":"Scarlet shouldered Parrotlet","size":16.6,"colname":"X2"},{"name":"Indian Vulture","size":16.3161,"colname":"X2"},{"name":"Eskimo Curlew","size":16,"colname":"X2"},{"name":"Red crowned Amazon","size":15.875,"colname":"X2"},{"name":"Royal Sunangel","size":15.8049,"colname":"X2"},{"name":"Little Woodstar","size":15.5122,"colname":"X2"},{"name":"Javan Green Magpie","size":15,"colname":"X2"},{"name":"Galapagos Hawk","size":14.696,"colname":"X2"},{"name":"Ridgway's Hawk","size":14.5933,"colname":"X2"},{"name":"Oaxaca Hummingbird","size":13.2683,"colname":"X2"},{"name":"Southern Bald Ibis","size":12.0712,"colname":"X2"},{"name":"Javan Cochoa","size":12,"colname":"X2"},{"name":"Sokoke Scops owl","size":12,"colname":"X2"},{"name":"Madagascar Fish eagle","size":11.8,"colname":"X2"},{"name":"Malherbe's Parakeet","size":11.2,"colname":"X2"},{"name":"Black cheeked Lovebird","size":11,"colname":"X2"},{"name":"Brown breasted Parakeet","size":10.8333,"colname":"X2"},{"name":"Red breasted Goose","size":10.8,"colname":"X2"},{"name":"Ecuadorian Piedtail","size":10.7038,"colname":"X2"},{"name":"Bare throated Bellbird","size":10,"colname":"X2"},{"name":"Yellow shouldered Amazon","size":10,"colname":"X2"},{"name":"Bulwer's Pheasant","size":9.0909,"colname":"X2"},{"name":"Hispaniolan Amazon","size":9.0712,"colname":"X2"},{"name":"Highland Guan","size":9,"colname":"X2"},{"name":"Blue throated Macaw","size":8.625,"colname":"X2"},{"name":"Dot eared Coquette","size":7.8293,"colname":"X2"},{"name":"Henderson Lorikeet","size":7,"colname":"X2"},{"name":"Ultramarine Lorikeet","size":7,"colname":"X2"},{"name":"Sulu Hornbill","size":6.5,"colname":"X2"},{"name":"White necked Parakeet","size":6.5,"colname":"X2"},{"name":"Red spectacled Amazon","size":6.4,"colname":"X2"},{"name":"Purple backed Sunbeam","size":6.3902,"colname":"X2"},{"name":"Fairy Pitta","size":6.3333,"colname":"X2"},{"name":"Grey necked Rockfowl","size":6.3333,"colname":"X2"},{"name":"Tucuman Amazon","size":6.3,"colname":"X2"},{"name":"Andean Flamingo","size":6.1333,"colname":"X2"},{"name":"Imperial Woodpecker","size":6,"colname":"X2"},{"name":"Hooded Crane","size":5.7971,"colname":"X2"},{"name":"Northern Bald Ibis","size":5.6667,"colname":"X2"},{"name":"Golden plumed Parakeet","size":5.6,"colname":"X2"},{"name":"Orange bellied Parrot","size":5.6,"colname":"X2"},{"name":"Fearful Owl","size":5,"colname":"X2"},{"name":"White headed Duck","size":5,"colname":"X2"},{"name":"Yellow faced Parrotlet","size":4.8,"colname":"X2"},{"name":"Grey cheeked Parakeet","size":4.4,"colname":"X2"},{"name":"Black winged Lory","size":4,"colname":"X2"},{"name":"Horned Guan","size":4,"colname":"X2"},{"name":"Narcondam Hornbill","size":4,"colname":"X2"},{"name":"Philippine Eagle owl","size":4,"colname":"X2"},{"name":"White fronted Scops owl","size":4,"colname":"X2"},{"name":"Antipodes Parakeet","size":3.4,"colname":"X2"},{"name":"White tailed Hummingbird","size":3.3171,"colname":"X2"},{"name":"Esmeraldas Woodstar","size":3.2,"colname":"X2"},{"name":"Rufous necked Hornbill","size":3,"colname":"X2"},{"name":"Short tailed Albatross","size":3,"colname":"X2"},{"name":"Sooty Falcon","size":2.8571,"colname":"X2"},{"name":"Black breasted Puffleg","size":2.8293,"colname":"X2"},{"name":"Glow throated Hummingbird","size":2.8293,"colname":"X2"},{"name":"West Indian Whistling duck","size":2.6,"colname":"X2"},{"name":"Pink throated Brilliant","size":2.4878,"colname":"X2"},{"name":"Relict Gull","size":2.4,"colname":"X2"},{"name":"Pallas's Fish eagle","size":2.2849,"colname":"X2"},{"name":"Gundlach's Hawk","size":2.16,"colname":"X2"},{"name":"Cheer Pheasant","size":2,"colname":"X2"},{"name":"Christmas Boobook","size":2,"colname":"X2"},{"name":"Emei Shan Liocichla","size":2,"colname":"X2"},{"name":"Golden Masked owl","size":2,"colname":"X2"},{"name":"Juan Fernandez Firecrown","size":2,"colname":"X2"},{"name":"Madagascar Red Owl","size":2,"colname":"X2"},{"name":"Minahasa Masked owl","size":2,"colname":"X2"},{"name":"Red and blue Lory","size":2,"colname":"X2"},{"name":"Sumba Hornbill","size":2,"colname":"X2"},{"name":"White crested Guan","size":2,"colname":"X2"},{"name":"Vinaceous breasted Amazon","size":1.92,"colname":"X2"},{"name":"Cabot's Tragopan","size":1.8182,"colname":"X2"},{"name":"Chinese Monal","size":1.8182,"colname":"X2"},{"name":"Edwards's Pheasant","size":1.8182,"colname":"X2"},{"name":"Mountain Peacock pheasant","size":1.8182,"colname":"X2"},{"name":"Palawan Peacock pheasant","size":1.8182,"colname":"X2"},{"name":"Ruspoli's Turaco","size":1.8,"colname":"X2"},{"name":"Spot winged Parrotlet","size":1.6,"colname":"X2"},{"name":"Red browed Amazon","size":1.4,"colname":"X2"},{"name":"Greater Adjutant","size":1,"colname":"X2"},{"name":"St Vincent Amazon","size":1,"colname":"X2"},{"name":"Western Tragopan","size":1,"colname":"X2"},{"name":"Grey Falcon","size":0.9524,"colname":"X2"},{"name":"Grey bellied Comet","size":0.8293,"colname":"X2"},{"name":"Mexican Woodnymph","size":0.8293,"colname":"X2"},{"name":"Violet throated Metaltail","size":0.8293,"colname":"X2"},{"name":"Brown backed Parrotlet","size":0.8,"colname":"X2"},{"name":"Maroon fronted Parrot","size":0.8,"colname":"X2"},{"name":"Red faced Parrot","size":0.8,"colname":"X2"},{"name":"Rufous fronted Parakeet","size":0.8,"colname":"X2"},{"name":"Rusty faced Parrot","size":0.8,"colname":"X2"},{"name":"Imitator Goshawk","size":0.48,"colname":"X2"},{"name":"Long whiskered Owlet","size":0.4706,"colname":"X2"},{"name":"Blue headed Racquet tail","size":0.4,"colname":"X2"},{"name":"White breasted Guineafowl","size":0.4,"colname":"X2"},{"name":"Yellow billed Amazon","size":0.32,"colname":"X2"},{"name":"Horned Parakeet","size":0.2849,"colname":"X2"},{"name":"Sanford's Sea eagle","size":0.16,"colname":"X2"},{"name":"Madagascar Serpent eagle","size":0.1425,"colname":"X2"},{"name":"Red necked Amazon","size":0.125,"colname":"X2"},{"name":"Black billed Amazon","size":0.12,"colname":"X2"}],"size":15952.0975,"colname":"X1"},{"name":"Mammalia","children":[{"name":"African Elephant","size":112042.8784,"colname":"X2"},{"name":"Fin Whale","size":85324.9412,"colname":"X2"},{"name":"Hippopotamus","size":63230.172,"colname":"X2"},{"name":"Blue Whale","size":33374.7927,"colname":"X2"},{"name":"Leopard","size":25030.6311,"colname":"X2"},{"name":"Sei Whale","size":16477.1442,"colname":"X2"},{"name":"Lion","size":15799.1952,"colname":"X2"},{"name":"Polar Bear","size":11007.3815,"colname":"X2"},{"name":"Asian Elephant","size":10499.3982,"colname":"X2"},{"name":"Walrus","size":8697.0439,"colname":"X2"},{"name":"Sperm Whale","size":7187.98,"colname":"X2"},{"name":"White lipped Peccary","size":5562.3679,"colname":"X2"},{"name":"Cheetah","size":3285.9614,"colname":"X2"},{"name":"Dorcas Gazelle","size":2009.1635,"colname":"X2"},{"name":"North Atlantic Right Whale","size":1200.8215,"colname":"X2"},{"name":"Verreaux's Sifaka","size":1176.1102,"colname":"X2"},{"name":"Chimpanzee","size":1077.9997,"colname":"X2"},{"name":"Bald headed Uacari","size":877.3299,"colname":"X2"},{"name":"Siberian Musk Deer","size":832.6497,"colname":"X2"},{"name":"Tiger","size":806.1525,"colname":"X2"},{"name":"Black Rhinoceros","size":806.02,"colname":"X2"},{"name":"Asiatic Black Bear","size":745.7491,"colname":"X2"},{"name":"Wild Water Buffalo","size":733.3653,"colname":"X2"},{"name":"Saiga","size":721.9529,"colname":"X2"},{"name":"La Plata River Dolphin","size":711.6868,"colname":"X2"},{"name":"Rufous Mouse Lemur","size":667.6141,"colname":"X2"},{"name":"Irrawaddy Dolphin","size":655.6613,"colname":"X2"},{"name":"Aoudad","size":620.575,"colname":"X2"},{"name":"Diademed Sifaka","size":599.5104,"colname":"X2"},{"name":"Western Gorilla","size":574.9838,"colname":"X2"},{"name":"Red tailed Sportive Lemur","size":530.1829,"colname":"X2"},{"name":"Fossa","size":468.8886,"colname":"X2"},{"name":"Indo Pacific Finless Porpoise","size":462.0082,"colname":"X2"},{"name":"Eastern Woolly Lemur","size":433.1597,"colname":"X2"},{"name":"Milne Edwards's Sportive Lemur","size":425.184,"colname":"X2"},{"name":"Indo Pacific Humpback Dolphin ","size":424.7954,"colname":"X2"},{"name":"Dugong","size":423.0706,"colname":"X2"},{"name":"Coquerel's Giant Mouse Lemur","size":414.7384,"colname":"X2"},{"name":"Diana Monkey","size":398.989,"colname":"X2"},{"name":"Daraina Sportive Lemur","size":388.3964,"colname":"X2"},{"name":"American Manatee","size":386.6401,"colname":"X2"},{"name":"White bellied Spider Monkey","size":337.95,"colname":"X2"},{"name":"Red capped Mangabey","size":336.3231,"colname":"X2"},{"name":"Sunda Pangolin","size":327.0041,"colname":"X2"},{"name":"Scimitar horned Oryx","size":325.8046,"colname":"X2"},{"name":"Mandrill","size":320.259,"colname":"X2"},{"name":"Tavaratra Mouse Lemur","size":312.5054,"colname":"X2"},{"name":"Southern Pig tailed Macaque","size":293.0899,"colname":"X2"},{"name":"Eastern Lesser Bamboo Lemur","size":284.7522,"colname":"X2"},{"name":"Geoffroy's Spider Monkey","size":283.8423,"colname":"X2"},{"name":"Nancy Ma's Night Monkey","size":272.9552,"colname":"X2"},{"name":"Eastern Gorilla","size":250.3942,"colname":"X2"},{"name":"Ring tailed Lemur","size":239.9147,"colname":"X2"},{"name":"Spotted Fanaloka","size":234.6032,"colname":"X2"},{"name":"Golden brown Mouse Lemur","size":229.2027,"colname":"X2"},{"name":"Livingstone's Flying Fox","size":225.775,"colname":"X2"},{"name":"Upper Guinea Red Colobus","size":224.4254,"colname":"X2"},{"name":"African Manatee","size":220.4154,"colname":"X2"},{"name":"Black faced Black Spider Monkey","size":212.9057,"colname":"X2"},{"name":"Red eared Monkey","size":212.5128,"colname":"X2"},{"name":"Eastern Falanouc","size":212.3822,"colname":"X2"},{"name":"Indri","size":206.0996,"colname":"X2"},{"name":"Bornean Orangutan","size":200.8846,"colname":"X2"},{"name":"White footed Sportive Lemur","size":193.9371,"colname":"X2"},{"name":"Ankarana Sportive Lemur","size":191.3986,"colname":"X2"},{"name":"Yucatán Black Howler Monkey","size":187.7739,"colname":"X2"},{"name":"Iberian Lynx","size":177.5276,"colname":"X2"},{"name":"Aye aye","size":177.4519,"colname":"X2"},{"name":"Black and white Ruffed Lemur","size":174.6024,"colname":"X2"},{"name":"Arabian Oryx","size":166.75,"colname":"X2"},{"name":"Variegated Spider Monkey","size":163.8205,"colname":"X2"},{"name":"Grevy's Zebra","size":158.5832,"colname":"X2"},{"name":"Giant Anteater","size":156.1425,"colname":"X2"},{"name":"King Colobus","size":151.0769,"colname":"X2"},{"name":"Guiana Spider Monkey","size":148.1911,"colname":"X2"},{"name":"Drill","size":142.5,"colname":"X2"},{"name":"Milne Edward's Sifaka","size":141.9972,"colname":"X2"},{"name":"Cotton headed Tamarin","size":141.5986,"colname":"X2"},{"name":"Madagascan Flying Fox","size":141.2847,"colname":"X2"},{"name":"Greater Bamboo Lemur","size":139.9594,"colname":"X2"},{"name":"Hector's Dolphin","size":139.9412,"colname":"X2"},{"name":"Red Brown Lemur","size":138.913,"colname":"X2"},{"name":"Sea Otter","size":133.58,"colname":"X2"},{"name":"Masoala Fork marked Lemur","size":129.3154,"colname":"X2"},{"name":"Black Bearded Saki","size":126.4,"colname":"X2"},{"name":"Golden Lion Tamarin","size":122.9496,"colname":"X2"},{"name":"Mediterranean Monk Seal","size":121.4124,"colname":"X2"},{"name":"Sibree's Dwarf Lemur","size":120.4203,"colname":"X2"},{"name":"Fishing Cat","size":116.54,"colname":"X2"},{"name":"Gray's Sportive Lemur","size":114.3152,"colname":"X2"},{"name":"Golden capped Fruit Bat","size":112.8062,"colname":"X2"},{"name":"Sumatran Rhinoceros","size":109.0476,"colname":"X2"},{"name":"Black Colobus","size":104.4872,"colname":"X2"},{"name":"Red bellied Lemur","size":104.0651,"colname":"X2"},{"name":"Alpine Musk Deer","size":103.2197,"colname":"X2"},{"name":"Dian's Tarsier","size":103.0909,"colname":"X2"},{"name":"Common Woolly Monkey","size":100.879,"colname":"X2"},{"name":"Mountain Zebra","size":99.9395,"colname":"X2"},{"name":"Greater Slow Loris","size":98.1219,"colname":"X2"},{"name":"Red shanked Douc Langur","size":92.0858,"colname":"X2"},{"name":"Chinese Pangolin","size":88.5824,"colname":"X2"},{"name":"Lowland Tapir","size":86.997,"colname":"X2"},{"name":"Madame Berthe's Mouse Lemur","size":85.4685,"colname":"X2"},{"name":"Sahafary Sportive Lemur","size":82.3717,"colname":"X2"},{"name":"Hairy Babirusa","size":79.8953,"colname":"X2"},{"name":"Lorenz Von Liburnau's Woolly Lemur","size":78.804,"colname":"X2"},{"name":"Takin","size":77.5,"colname":"X2"},{"name":"Black Lemur","size":75.3082,"colname":"X2"},{"name":"Barbary Macaque","size":74.7357,"colname":"X2"},{"name":"Pennant's Red Colobus","size":74.6667,"colname":"X2"},{"name":"Bonobo","size":74.2824,"colname":"X2"},{"name":"Poeppig's Woolly Monkey","size":74.1573,"colname":"X2"},{"name":"Northern Tiger Cat","size":74.0516,"colname":"X2"},{"name":"Baird's Tapir","size":72.4944,"colname":"X2"},{"name":"Southern Muriqui","size":71.9417,"colname":"X2"},{"name":"Clouded Leopard","size":70.7429,"colname":"X2"},{"name":"Malabar Civet","size":70,"colname":"X2"},{"name":"Golden Snub nosed Monkey","size":69.7993,"colname":"X2"},{"name":"Small toothed Sportive Lemur","size":69.3721,"colname":"X2"},{"name":"Black footed Cat","size":69.2463,"colname":"X2"},{"name":"Spectacled Bear","size":69.1784,"colname":"X2"},{"name":"Red Ruffed Lemur","size":68.0469,"colname":"X2"},{"name":"Peters' Mouse Lemur","size":67.2306,"colname":"X2"},{"name":"Binturong","size":64,"colname":"X2"},{"name":"Siamang","size":63.6482,"colname":"X2"},{"name":"Crowned Lemur","size":63.146,"colname":"X2"},{"name":"South Asian River Dolphin","size":59.1449,"colname":"X2"},{"name":"White fronted Lemur","size":58.4981,"colname":"X2"},{"name":"Patagonian Huemul","size":58.1529,"colname":"X2"},{"name":"Sambirano Mouse Lemur","size":54.2799,"colname":"X2"},{"name":"Montagne D' Ambre Fork marked Lemur","size":53.7241,"colname":"X2"},{"name":"Simmons' Mouse Lemur","size":53.0553,"colname":"X2"},{"name":"Giant Panda","size":52.7993,"colname":"X2"},{"name":"Indian Rhinoceros","size":50.9167,"colname":"X2"},{"name":"Mexican Prairie Dog","size":50,"colname":"X2"},{"name":"Spectral Tarsier","size":49.1425,"colname":"X2"},{"name":"Colombian Night Monkey","size":48.1158,"colname":"X2"},{"name":"Giant Armadillo","size":47.7349,"colname":"X2"},{"name":"Mongoose Lemur","size":45.5138,"colname":"X2"},{"name":"Celebes Crested Macaque","size":44.8755,"colname":"X2"},{"name":"Addax","size":44,"colname":"X2"},{"name":"Golden Bamboo Lemur","size":43.0477,"colname":"X2"},{"name":"Javan Surili","size":41.6312,"colname":"X2"},{"name":"Coimbra Filho's Titi Monkey","size":40,"colname":"X2"},{"name":"Hog Deer","size":40,"colname":"X2"},{"name":"Tana River Red Colobus","size":39.6398,"colname":"X2"},{"name":"Amazonian Manatee","size":38.8919,"colname":"X2"},{"name":"Flat headed Cat","size":36.9259,"colname":"X2"},{"name":"Lowland Anoa","size":36.375,"colname":"X2"},{"name":"Sumatran Orangutan","size":35.7767,"colname":"X2"},{"name":"Talaud Fruit Bat","size":35,"colname":"X2"},{"name":"North Pacific Right Whale","size":34.1137,"colname":"X2"},{"name":"Lyle's Flying Fox","size":32.2353,"colname":"X2"},{"name":"Sun Bear","size":32.0124,"colname":"X2"},{"name":"Dryas Monkey","size":32,"colname":"X2"},{"name":"Giant Otter","size":31.5,"colname":"X2"},{"name":"Mountain Anoa","size":31.25,"colname":"X2"},{"name":"Otter Civet","size":30.6667,"colname":"X2"},{"name":"Silvery Gibbon","size":28.3005,"colname":"X2"},{"name":"Northern Muriqui","size":28,"colname":"X2"},{"name":"Marine Otter","size":26.1,"colname":"X2"},{"name":"Huon Tree Kangaroo","size":24.5711,"colname":"X2"},{"name":"Pygmy Slow Loris","size":24,"colname":"X2"},{"name":"Rodrigues Flying Fox","size":23.8235,"colname":"X2"},{"name":"African Wild Ass","size":22.4431,"colname":"X2"},{"name":"Golden crowned Sifaka","size":22.1925,"colname":"X2"},{"name":"Proboscis Monkey","size":22.164,"colname":"X2"},{"name":"Greater Mascarene Flying Fox","size":21.6471,"colname":"X2"},{"name":"Javan Rhinoceros","size":21.4046,"colname":"X2"},{"name":"Gaur","size":21.3714,"colname":"X2"},{"name":"Grizzled Tree Kangaroo","size":21.1425,"colname":"X2"},{"name":"Red cheeked Gibbon","size":20.6744,"colname":"X2"},{"name":"Lar Gibbon","size":20.5894,"colname":"X2"},{"name":"Marsh Deer","size":20.407,"colname":"X2"},{"name":"Owl faced Monkey","size":20.2143,"colname":"X2"},{"name":"Grey headed Flying Fox","size":19.1765,"colname":"X2"},{"name":"Collared Brown Lemur","size":18.6872,"colname":"X2"},{"name":"Agile Gibbon","size":17.913,"colname":"X2"},{"name":"Sulawesi Fruit Bat","size":14.1231,"colname":"X2"},{"name":"Malay Tapir","size":14,"colname":"X2"},{"name":"Indochinese Lutung","size":13.6923,"colname":"X2"},{"name":"Black crowned Central American Squirrel Monkey","size":12.8483,"colname":"X2"},{"name":"Zebra Duiker","size":12.1425,"colname":"X2"},{"name":"New Caledonia Flying Fox","size":12.1033,"colname":"X2"},{"name":"Javan Lutung","size":12.0171,"colname":"X2"},{"name":"Taruca","size":10.4113,"colname":"X2"},{"name":"Sumatran Serow","size":10.3075,"colname":"X2"},{"name":"Ornate Flying Fox","size":9.718,"colname":"X2"},{"name":"Pig tailed Langur","size":9.7113,"colname":"X2"},{"name":"Tana River Mangabey","size":9.2663,"colname":"X2"},{"name":"Galápagos Fur Seal","size":8.7632,"colname":"X2"},{"name":"Temminck's Flying Fox","size":8.4706,"colname":"X2"},{"name":"Ryukyu Flying Fox","size":8.1765,"colname":"X2"},{"name":"Pileated Gibbon","size":8.087,"colname":"X2"},{"name":"Atlantic Titi","size":8,"colname":"X2"},{"name":"Blond Titi Monkey","size":8,"colname":"X2"},{"name":"Indian Ocean Humpback Dolphin","size":8,"colname":"X2"},{"name":"Pale Fork marked Lemur","size":7.8904,"colname":"X2"},{"name":"Peruvian Yellow tailed Woolly Monkey","size":7.5176,"colname":"X2"},{"name":"Saola","size":7.25,"colname":"X2"},{"name":"Red Panda","size":7.125,"colname":"X2"},{"name":"Silvery brown Tamarin","size":6.9412,"colname":"X2"},{"name":"Phayre's Leaf monkey","size":6.9231,"colname":"X2"},{"name":"Philippine Pangolin","size":6.8,"colname":"X2"},{"name":"Goeldi's Monkey","size":6.6471,"colname":"X2"},{"name":"Moluccan Flying Fox","size":6.5882,"colname":"X2"},{"name":"Black footed Ferret","size":6.5455,"colname":"X2"},{"name":"Coastal Black handed Titi","size":6.4,"colname":"X2"},{"name":"Sunda Fruit Bat","size":6.2462,"colname":"X2"},{"name":"Barasingha","size":6.2142,"colname":"X2"},{"name":"Stump tailed Macaque","size":6.1911,"colname":"X2"},{"name":"Dama Gazelle","size":6,"colname":"X2"},{"name":"Thylacine","size":5.825,"colname":"X2"},{"name":"Ashy Red Colobus","size":5.7025,"colname":"X2"},{"name":"White thighed Colobus","size":5.6364,"colname":"X2"},{"name":"Eld's Deer","size":5.4465,"colname":"X2"},{"name":"Mountain Tapir","size":5.4286,"colname":"X2"},{"name":"Tonkean Macaque","size":5.4286,"colname":"X2"},{"name":"Heck's Macaque","size":5.1429,"colname":"X2"},{"name":"Chacoan Peccary","size":4.9028,"colname":"X2"},{"name":"Australian Snubfin Dolphin","size":4.8,"colname":"X2"},{"name":"Kloss's Gibbon","size":4.4783,"colname":"X2"},{"name":"François's Langur","size":4.3077,"colname":"X2"},{"name":"Guiña","size":4.2727,"colname":"X2"},{"name":"Lion tailed Macaque","size":4.0712,"colname":"X2"},{"name":"Golden headed Lion Tamarin","size":4,"colname":"X2"},{"name":"San Martin Titi Monkey","size":3.7698,"colname":"X2"},{"name":"Wild Yak","size":3.7077,"colname":"X2"},{"name":"Coquerel's Sifaka","size":3.6183,"colname":"X2"},{"name":"North Moluccan Flying Fox","size":3.5294,"colname":"X2"},{"name":"Seram Flying Fox","size":3.2941,"colname":"X2"},{"name":"Indian Pangolin","size":3.2,"colname":"X2"},{"name":"Northern White cheeked Gibbon","size":3.0712,"colname":"X2"},{"name":"Bilby","size":3,"colname":"X2"},{"name":"Red Slender Loris","size":2.8929,"colname":"X2"},{"name":"Sanje River Mangabey","size":2.8579,"colname":"X2"},{"name":"Hairy eared Dwarf Lemur","size":2.7686,"colname":"X2"},{"name":"Peruvian Woolly Monkey","size":2.6778,"colname":"X2"},{"name":"Sloth Bear","size":2.6,"colname":"X2"},{"name":"Horsfield's Tarsier","size":2.4476,"colname":"X2"},{"name":"Perrier's Sifaka","size":2.4462,"colname":"X2"},{"name":"Vanauatu Flying Fox","size":2.4,"colname":"X2"},{"name":"Sambirano Fork marked Lemur","size":2.3774,"colname":"X2"},{"name":"Aldabra Flying fox","size":2.3529,"colname":"X2"},{"name":"Goodfellow's Tree Kangaroo","size":2.3167,"colname":"X2"},{"name":"Jentink's Duiker","size":2.25,"colname":"X2"},{"name":"Vaquita","size":2.25,"colname":"X2"},{"name":"Northern Pudu","size":2.2324,"colname":"X2"},{"name":"Borneo Bay Cat","size":2.1425,"colname":"X2"},{"name":"Black bearded Flying Fox","size":2.1176,"colname":"X2"},{"name":"Seal's Sportive Lemur","size":2.0438,"colname":"X2"},{"name":"Alaotra Reed Lemur","size":2,"colname":"X2"},{"name":"Nilgiri Langur","size":2,"colname":"X2"},{"name":"Southern River Otter","size":1.722,"colname":"X2"},{"name":"Darwin's Fox","size":1.6579,"colname":"X2"},{"name":"Northern Giant Mouse Lemur","size":1.5,"colname":"X2"},{"name":"Eastern Long beaked Echidna","size":1.5,"colname":"X2"},{"name":"Kouprey","size":1.5,"colname":"X2"},{"name":"Slender horned Gazelle","size":1.5,"colname":"X2"},{"name":"Sanford's Brown Lemur","size":1.421,"colname":"X2"},{"name":"Sambirano Lesser Bamboo Lemur","size":1.3896,"colname":"X2"},{"name":"Sumatran Surili","size":1.3758,"colname":"X2"},{"name":"Kipunji","size":1.3237,"colname":"X2"},{"name":"Red handed Howler Monkey","size":1.2571,"colname":"X2"},{"name":"Black Lion Tamarin","size":1.1765,"colname":"X2"},{"name":"Black Crested Gibbon","size":1.093,"colname":"X2"},{"name":"Buffy tufted ear Marmoset","size":1,"colname":"X2"},{"name":"Golden mantled Tree Kangaroo","size":1,"colname":"X2"},{"name":"Smooth coated Otter","size":1,"colname":"X2"},{"name":"Tamaraw","size":1,"colname":"X2"},{"name":"Toque Macaque","size":1,"colname":"X2"},{"name":"Brazilian Bare faced Tamarin","size":0.9976,"colname":"X2"},{"name":"Hairy nosed Otter","size":0.9091,"colname":"X2"},{"name":"Van der Decken's Sifaka","size":0.8133,"colname":"X2"},{"name":"Pygmy Three toed Sloth","size":0.8,"colname":"X2"},{"name":"Pemba Flying Fox","size":0.7059,"colname":"X2"},{"name":"Ka'apor Capuchin","size":0.6379,"colname":"X2"},{"name":"Brown headed Spider Monkey","size":0.45,"colname":"X2"},{"name":"Guam Flying Fox","size":0.4,"colname":"X2"},{"name":"Temotu Flying Fox","size":0.4,"colname":"X2"},{"name":"Black faced Lion Tamarin","size":0.3529,"colname":"X2"},{"name":"Atlantic Humpbacked Dolphin","size":0.3519,"colname":"X2"},{"name":"Forest Musk Deer","size":0.3395,"colname":"X2"},{"name":"White cheeked Spider Monkey","size":0.3205,"colname":"X2"},{"name":"Purple faced Langur","size":0.3077,"colname":"X2"},{"name":"Northern Pig tailed Macaque","size":0.2857,"colname":"X2"},{"name":"Doria's Tree Kangaroo","size":0.1667,"colname":"X2"},{"name":"Dhole","size":0.1579,"colname":"X2"},{"name":"Andean Night Monkey","size":0.1425,"colname":"X2"},{"name":"Banded Hare Wallaby","size":0.1425,"colname":"X2"},{"name":"Chinese Mountain Cat","size":0.1425,"colname":"X2"},{"name":"Sir David's Long beaked Echidna","size":0.1425,"colname":"X2"},{"name":"Bengal Slow Loris","size":0.0857,"colname":"X2"}],"size":18480.4023,"colname":"X1"},{"name":"Bivalvia","children":[{"name":"Southern Giant Clam","size":81369.0851,"colname":"X2"},{"name":"Giant Clam","size":27137.059,"colname":"X2"},{"name":"Club Naiad","size":112.5,"colname":"X2"},{"name":"Dromedary Naiad","size":112.5,"colname":"X2"}],"size":65854.2833,"colname":"X1"},{"name":"Amphibia","children":[{"name":"Madagascan Mantella","size":33305.0638,"colname":"X2"},{"name":"Golden Mantella","size":26874.4266,"colname":"X2"},{"name":"Blue legged Mantella","size":16089,"colname":"X2"},{"name":"Green Golden Frog","size":14976.2222,"colname":"X2"},{"name":"Black eared Mantella","size":14067.4444,"colname":"X2"},{"name":"Eastern Golden Frog","size":12019.9556,"colname":"X2"},{"name":"Malagasy rainbow frog","size":7161,"colname":"X2"},{"name":"Cowan's Mantella","size":3906.3889,"colname":"X2"},{"name":"Bernhard's Mantella","size":3602.5,"colname":"X2"},{"name":"Haraldmeier's Mantella","size":2104,"colname":"X2"},{"name":"Giant Slippery Frog","size":914.5,"colname":"X2"},{"name":"Red tailed Knobby Newt","size":858,"colname":"X2"},{"name":"Cerro Campana Stubfoot Toad","size":677.6,"colname":"X2"},{"name":"Mount Nimba Viviparous Toad","size":544.32,"colname":"X2"},{"name":"Granular Poison Frog","size":504.8531,"colname":"X2"},{"name":"Sword tailed Newt","size":213,"colname":"X2"},{"name":"Golden Poison Frog","size":177.7127,"colname":"X2"},{"name":"Axolotl","size":158,"colname":"X2"},{"name":"Epipedobates tricolor","size":108,"colname":"X2"},{"name":"Laotriton laoensis","size":41,"colname":"X2"},{"name":"Silverstone's Poison Frog","size":29.2222,"colname":"X2"},{"name":"Golfodulcean Poison Frog","size":15.2222,"colname":"X2"},{"name":"Cainarachi Poison Frog","size":11.1111,"colname":"X2"},{"name":"Tylototriton vietnamensis","size":10,"colname":"X2"},{"name":"Summers' Poison Frog","size":7.7778,"colname":"X2"},{"name":"Hynobius amjiensis","size":6.9,"colname":"X2"},{"name":"Minute Tree Toad","size":6.4107,"colname":"X2"},{"name":"Chinese Giant Salamander","size":5.9,"colname":"X2"},{"name":"Sky Blue Poison Dart Frog","size":4.1111,"colname":"X2"},{"name":"Kihansi Spray Toad","size":3.2054,"colname":"X2"},{"name":"Nectophrynoides cryptus","size":3.2054,"colname":"X2"}],"size":16721.5057,"colname":"X1"},{"name":"Hydrozoa","children":[{"name":"Millepora striata","size":3,"colname":"X2"}],"size":116370.6401,"colname":"X1"}],"name":"root"},"options":{"legendOrder":null,"colors":{"domain":["Anthozoa","Actinopteri","Holothuroidea","Reptilia","Aves","Mammalia","Bivalvia","Amphibia","Hydrozoa","Heliofungia actiniformis","Catalaphyllia jardinei","Euphyllia ancora","Euphyllia cristata","Turbinaria peltata","Turbinaria mesenterina","Physogyra lichtensteini","Blue Coral","Galaxea astreata","Turbinaria reniformis","Euphyllia paraancora","Porites nigrescens","Elkhorn Coral","Pachyseris rugosa","Euphyllia paradivisa","Moseleya latistellata","Lettuce Coral","Staghorn Coral","Acropora echinata","Acropora horrida","Acropora aspera","Acanthastrea bowerbanki","Acropora microclados","Montipora capricornis","Cactus Coral","Alveopora gigas","Pavona cactus","Acropora vaughani","Acropora pharaonis","Acropora abrolhosensis","Acropora aculeus","Acropora paniculata","Acropora anthocercis","Acropora caroliniana","Acropora jacquelineae","Australogyra zelli","Acropora palmerae","Acropora kimbeensis","Acropora hemprichii","Acropora polystoma","Acropora suharsonoi","Acropora papillare","Acropora hoeksemai","Cyphastrea ocellina","Acropora desalwii","Acropora donei","Acropora batunai","Porites horizontalata","Acropora loisetteae","Acropora verweyi","Pocillopora elegans","Acropora multiacuta","Acanthastrea hemprichii","Fungia curvata","Psammocora stellata","Acropora listeri","Elliptical Star Coral","Pocillopora danae","Acropora speciosa","Lobophyllia diminuta","Acropora turaki","Turbinaria heronensis","Acropora simplex","Montipora caliculata","Plerogyra discus","Pectinia alcicornis","Large Ivory Coral","Acropora solitaryensis","Montipora calcarea","Leptoria irregularis","Acropora willisae","Acropora kosurini","Leptoseris yabei","Pavona venosa","Alveopora excelsa","Acropora dendrum","Mediterranean Pillow Coral","Rough Cactus Coral","Pillar Coral","Acropora lokani","Porites attenuata","Turbinaria stellulata","Acropora acuminata","Acropora lovelli","Goniopora albiconus","Acropora globiceps","Acropora walindii","Acropora awi","Montipora angulata","Acropora roseni","Acropora elegans","Turbinaria bifrons","Montipora samarensis","Lamarck's Sheet Coral","Turbinaria patula","Anomastraea irregularis","Montipora australiensis","Echinophyllia costata","Polycyathus isabela","Acropora striata","Alveopora verrilliana","Alveopora allingi","Montipora cocosensis","Montipora stilosa","Leptoseris incrustans","Acanthastrea faviaformis","Acanthastrea ishigakiensis","Pavona bipartita","Montipora cebuensis","Symphyllia hassi","Floreana Coral","Acropora retusa","Acropora tenella","Anacropora spinosa","Pavona danai","Echinopora ashmorensis","Hydnophora bonsai","Porites cumulatus","Favites spinosa","Acropora russelli","Galaxea acrhelia","Leptastrea aequalis","Lobophyllia dentatus","Montipora patula","Mycedium steeni","Acropora kirstyae","Montastrea salebrosa","Acropora spicifera","Astreopora incrustans","Siderastrea glynni","Lobophyllia flabelliformis","Montipora altasepta","Acanthastrea regularis","Acropora derawanensis","Acropora indonesia","Ctenella chagius","Montastrea multipunctata","Montipora crassituberculata","Montipora setosa","Astreopora cucullata","Fungia taiwanensis","Platygyra yaeyamaensis","Porites aranetai","Porites okinawensis","European Eel","Giant Seahorse","Spotted Seahorse","Banggai Cardinalfish","Giant Wrasse","Stellate Sturgeon","Beluga","Persian Sturgeon","Russian Sturgeon","Three spot Seahorse","Hedgehog Seahorse","Great Seahorse","Thorny Seahorse","Asian Arowana","Lined Seahorse","Barbour's Seahorse","Tiger Tail Seahorse","Paddlefish","West African Seahorse","Siberian Sturgeon","Sterlet","Kaluga","Amur Sturgeon","White's Seahorse","Shovelnose Sturgeon","Ship Sturgeon","False Shovelnose Sturgeon","Japanese Seahorse","Atlantic Sturgeon","Dwarf Sturgeon","Congo Blind Barb","Pallid Sturgeon","Shortnose Sturgeon","Totoaba","Brown Sea Cucumber","Afghan Tortoise","Southeast Asian Box Turtle","Asiatic Softshell Turtle","Alligator Snapping Turtle","Chinese Stripe necked Turtle","Black Marsh Turtle","Giant Asian Pond Turtle","Burmese Python","Common Tortoise","Yellow headed Temple Turtle","Reeves' Turtle","Malaysian Giant Turtle","Spiny Turtle","Indian Star Tortoise","Siamese Crocodile","Vietnamese Pond Turtle","Home's Hinge back Tortoise","Uroplatus ebenaui","Four horned Chameleon","Crested Gecko","Malayan Flat shelled Turtle","Egyptian Mastigure","Painted Terrapin","Yellow Pond Turtle","Green Turtle","Kemp's Ridley","Elongated Tortoise","Crevice Tortoise","Uroplatus henkeli","Celebes Tortoise","Asian Giant Tortoise","Olive Ridley","King Cobra","Chinese Softshell Turtle","Hawksbill Turtle","Spider Tortoise","African Spurred Tortoise","Yellow spotted River Turtle","Keeled Box Turtle","Chinese Red necked Turtle","Mekong Snail eating Turtle","Uroplatus pietschmanni","Decary's Leaf Chameleon","Ploughshare Tortoise","Palawan Forest Turtle","Flat tailed Tortoise","Loggerhead Turtle","Central American River Turtle","Black breasted Leaf Turtle","Leatherback","Uroplatus guentheri","Lesser Antillean Green Iguana","Four lined Girdled Lizard","Laborde's Chameleon","New Guinea Giant Softshell Turtle","Madagascar Big headed Turtle","Furcifer campani","Brookesia minima","Island Day Gecko","Ringed Sawback","Radiated Tortoise","Meadow Viper","Furcifer rhinoceratus","Tiger Chameleon","Asian Giant Softshell Turtle","Eiongate Leaf Chameleon","Bakossi Two horned Chameleon","O'Shaughnessy's Chameleon","Sulawesi Forest Turtle","Yellow margined Box Turtle","Chaco Tortoise","Fiji Crested Iguana","African Softshell Turtle","Indochinese Box Turtle ","Chinese Three striped Box Turtle","Uroplatus malama","Coahuila Box Turtle","Round Island Day Gecko","Northern Leaf Chameleon","Antimena Chameleon","Impressed Tortoise","Red headed Amazon River Turtle","Petter's Chameleon","Furcifer nicosiai","Wood Turtle","Natal Midlands Dwarf Chameleon","American Crocodile","Pig nosed Turtle","Phelsuma antanosy","Asian Narrow headed Softshell Turtle","Mojave Desert Tortoise","Fernandina Marine Iguana","Big headed Amazon River Turtle","Round Island Keel scaled Boa","Burmese Starred Tortoise","Northern Bahamian Rock Iguana","Big headed Turtle","Antsingy Leaf Chameleon","Egyptian Tortoise","Calumma furcifer","Eastern Box Turtle","Calumma globifer","Brookesia peyrierasi","Durban Dwarf Chameleon","African Dwarf Crocodile","Phelsuma serraticauda","Mount d'Ambre Leaf Chameleon","Common Land Iguana","Mertens' Day Gecko","Yellow headed Box Turtle","Roatán Spiny tailed Iguana","Black chested Spiny tailed Iguana","Brookesia vadoni","Turks and Caicos Rock Iguana","Brookesia karchei","Utila Spiny tailed Iguana","McCord's Box Turtle","Giant Dragon Lizard","Panay Monitor Lizard","Magdalena River Turtle","Chinese Cobra","Floreana Giant Tortoise","Geometric Tortoise","Anegada Rock Iguana","Standing's Day Gecko","Cape Dwarf Chameleon","Burmese Roofed Turtle","Jamaican Iguana","Spiny sided Chameleon","Four eyed Turtle","Usambara Spiny Pygmy Chameleon","Elandsberg Dwarf Chameleon","Pondo Dwarf Chameleon","Clouded Rock Iguana","Northern River Terrapin","Yellow throated Day Gecko","Blanding's Turtle","Calumma gallus","Perret's Montane Chameleon","Terrestrial Arboreal Alligator Lizard","Lesser Chameleon","Orinoco Crocodile","Six tubercled Amazon River Turtle","Blue Speckled Tree Monitor ","Philippine Crocodile","Grand Cayman Blue Iguana","Tsaratanana Chameleon","Trioceros serratus","Chinese Crocodile Lizard","Brookesia valerieae","Pan's Box Turtle","Two banded Chameleon","Lau Banded Iguana","Komodo Dragon","Phelsuma breviceps","Brookesia dentata","Speckled Dwarf Tortoise","Hispaniolan Rhinoceros Iguana","Central Bahamian Rock Iguana","Galápagos Pink Land Iguana","Dwarf Crag Lizard","Usambara Flap nosed Chameleon","Santa Fe Land Iguana","Exiliboa placata","Calumma hilleniusi","Gray's Monitor","Southern River Terrapin","Cuban Crocodile","Assam Roofed Turtle","Brookesia exarmata","Phelsuma seippi","Uroplatus malahelo","Vences' Chameleon","Omani Spiny tailed Lizard","Spotted Pond Turtle","False Gharial","Brookesia ramanantsoai","Calumma glawi","Phelsuma klemmeri","Arakan Forest Turtle","Phelsuma gigas","Phelsuma hielscheri","Belalanda Chameleon","Indian Narrow headed Softshell Turtle","Calumma andringitraense","Calumma peyrierasi","Calumma cucullatum","Uroplatus giganteus","Gopher Tortoise","Calumma capuroni","Pascagoula Map Turtle","Red crowned Roofed Turtle","Python kyaiktiyo","Black And White Spitting Cobra","Roti Island Snake necked Turtle","Gharial","Bog Turtle","Phelsuma pronki","Fiji Banded Iguana","Mugger","Bolson Tortoise","Grey Parrot","Pink Pigeon","European Turtle dove","Saker Falcon","Java Sparrow","Cape Parrot","Channel billed Toucan","Red billed Toucan","Scheepmaker's Crowned pigeon","Asian Houbara","Great Bustard","Spanish Imperial Eagle","Sun Parakeet","White backed Vulture","Great Curassow","Black Crowned crane","El Oro Parakeet","White Cockatoo","Yellow Cardinal","Mauritius Kestrel","Hooded Vulture","Nicobar Sparrowhawk","African Penguin","African Houbara","Abbott's Booby","Humboldt Penguin","Lilac crowned Amazon","Yellow naped Amazon","Christmas Frigatebird","White headed Vulture","Secretarybird","Crimson bellied Parakeet","Yellow headed Amazon","Grey Crowned crane","Chattering Lory","Yellow crested Cockatoo","Red headed Vulture","Rimatara Lorikeet","White rumped Vulture","Egyptian Vulture","Goias Parakeet","Long wattled Umbrellabird","Black Harrier","Banded Cotinga","Lear's Macaw","New Zealand Kaka","Reeves's Pheasant","Oriental Stork","Martial Eagle","Cape Vulture","Swift Parrot","Hyacinth Macaw","White necked Rockfowl","Alagoas Curassow","Helmeted Curassow","Seven colored Tanager","Whooping Crane","Visayan Hornbill","White eared Parakeet","Kakapo","Chatham Parakeet","Green Peafowl","Steppe Eagle","Kea","Slender billed Vulture","Shoebill","Green thighed Parrot","Eastern Imperial Eagle","Goldie's Bird of paradise","Steller's Sea eagle","Thick billed Parrot","Noisy Scrub bird","Papuan Eagle","Meller's Duck","Bali Myna","Blakiston's Eagle owl","Ludwig's Bustard","White winged Cotinga","Bengal Florican","Great Green Macaw","Salmon crested Cockatoo","Blue Crane","California Condor","Northern Rufous Hornbill","Southern Black Bustard","Spix's Macaw","St Lucia Amazon","Pearly Parakeet","Red crowned Crane","Western Crowned pigeon","Black Curassow","Great Indian Bustard","Lesser Florican","Storm's Stork","Asian Crested Ibis","Pesquet's Parrot","Philippine Cockatoo","Blue eyed Cockatoo","Trinidad Piping guan","Red fronted Macaw","Purple naped Lory","Philippine Eagle","Wattled Crane","Palm Lorikeet","Madagascar Teal","Military Macaw","Helmeted Hornbill","Malay Crestless Fireback","Malay Peacock pheasant","Blue headed Macaw","Campbell Teal","Palawan Hornbill","White winged Guan","Seychelles Kestrel","Wattled Curassow","Scarlet shouldered Parrotlet","Indian Vulture","Eskimo Curlew","Red crowned Amazon","Royal Sunangel","Little Woodstar","Javan Green Magpie","Galapagos Hawk","Ridgway's Hawk","Oaxaca Hummingbird","Southern Bald Ibis","Javan Cochoa","Sokoke Scops owl","Madagascar Fish eagle","Malherbe's Parakeet","Black cheeked Lovebird","Brown breasted Parakeet","Red breasted Goose","Ecuadorian Piedtail","Bare throated Bellbird","Yellow shouldered Amazon","Bulwer's Pheasant","Hispaniolan Amazon","Highland Guan","Blue throated Macaw","Dot eared Coquette","Henderson Lorikeet","Ultramarine Lorikeet","Sulu Hornbill","White necked Parakeet","Red spectacled Amazon","Purple backed Sunbeam","Fairy Pitta","Grey necked Rockfowl","Tucuman Amazon","Andean Flamingo","Imperial Woodpecker","Hooded Crane","Northern Bald Ibis","Golden plumed Parakeet","Orange bellied Parrot","Fearful Owl","White headed Duck","Yellow faced Parrotlet","Grey cheeked Parakeet","Black winged Lory","Horned Guan","Narcondam Hornbill","Philippine Eagle owl","White fronted Scops owl","Antipodes Parakeet","White tailed Hummingbird","Esmeraldas Woodstar","Rufous necked Hornbill","Short tailed Albatross","Sooty Falcon","Black breasted Puffleg","Glow throated Hummingbird","West Indian Whistling duck","Pink throated Brilliant","Relict Gull","Pallas's Fish eagle","Gundlach's Hawk","Cheer Pheasant","Christmas Boobook","Emei Shan Liocichla","Golden Masked owl","Juan Fernandez Firecrown","Madagascar Red Owl","Minahasa Masked owl","Red and blue Lory","Sumba Hornbill","White crested Guan","Vinaceous breasted Amazon","Cabot's Tragopan","Chinese Monal","Edwards's Pheasant","Mountain Peacock pheasant","Palawan Peacock pheasant","Ruspoli's Turaco","Spot winged Parrotlet","Red browed Amazon","Greater Adjutant","St Vincent Amazon","Western Tragopan","Grey Falcon","Grey bellied Comet","Mexican Woodnymph","Violet throated Metaltail","Brown backed Parrotlet","Maroon fronted Parrot","Red faced Parrot","Rufous fronted Parakeet","Rusty faced Parrot","Imitator Goshawk","Long whiskered Owlet","Blue headed Racquet tail","White breasted Guineafowl","Yellow billed Amazon","Horned Parakeet","Sanford's Sea eagle","Madagascar Serpent eagle","Red necked Amazon","Black billed Amazon","African Elephant","Fin Whale","Hippopotamus","Blue Whale","Leopard","Sei Whale","Lion","Polar Bear","Asian Elephant","Walrus","Sperm Whale","White lipped Peccary","Cheetah","Dorcas Gazelle","North Atlantic Right Whale","Verreaux's Sifaka","Chimpanzee","Bald headed Uacari","Siberian Musk Deer","Tiger","Black Rhinoceros","Asiatic Black Bear","Wild Water Buffalo","Saiga","La Plata River Dolphin","Rufous Mouse Lemur","Irrawaddy Dolphin","Aoudad","Diademed Sifaka","Western Gorilla","Red tailed Sportive Lemur","Fossa","Indo Pacific Finless Porpoise","Eastern Woolly Lemur","Milne Edwards's Sportive Lemur","Indo Pacific Humpback Dolphin ","Dugong","Coquerel's Giant Mouse Lemur","Diana Monkey","Daraina Sportive Lemur","American Manatee","White bellied Spider Monkey","Red capped Mangabey","Sunda Pangolin","Scimitar horned Oryx","Mandrill","Tavaratra Mouse Lemur","Southern Pig tailed Macaque","Eastern Lesser Bamboo Lemur","Geoffroy's Spider Monkey","Nancy Ma's Night Monkey","Eastern Gorilla","Ring tailed Lemur","Spotted Fanaloka","Golden brown Mouse Lemur","Livingstone's Flying Fox","Upper Guinea Red Colobus","African Manatee","Black faced Black Spider Monkey","Red eared Monkey","Eastern Falanouc","Indri","Bornean Orangutan","White footed Sportive Lemur","Ankarana Sportive Lemur","Yucatán Black Howler Monkey","Iberian Lynx","Aye aye","Black and white Ruffed Lemur","Arabian Oryx","Variegated Spider Monkey","Grevy's Zebra","Giant Anteater","King Colobus","Guiana Spider Monkey","Drill","Milne Edward's Sifaka","Cotton headed Tamarin","Madagascan Flying Fox","Greater Bamboo Lemur","Hector's Dolphin","Red Brown Lemur","Sea Otter","Masoala Fork marked Lemur","Black Bearded Saki","Golden Lion Tamarin","Mediterranean Monk Seal","Sibree's Dwarf Lemur","Fishing Cat","Gray's Sportive Lemur","Golden capped Fruit Bat","Sumatran Rhinoceros","Black Colobus","Red bellied Lemur","Alpine Musk Deer","Dian's Tarsier","Common Woolly Monkey","Mountain Zebra","Greater Slow Loris","Red shanked Douc Langur","Chinese Pangolin","Lowland Tapir","Madame Berthe's Mouse Lemur","Sahafary Sportive Lemur","Hairy Babirusa","Lorenz Von Liburnau's Woolly Lemur","Takin","Black Lemur","Barbary Macaque","Pennant's Red Colobus","Bonobo","Poeppig's Woolly Monkey","Northern Tiger Cat","Baird's Tapir","Southern Muriqui","Clouded Leopard","Malabar Civet","Golden Snub nosed Monkey","Small toothed Sportive Lemur","Black footed Cat","Spectacled Bear","Red Ruffed Lemur","Peters' Mouse Lemur","Binturong","Siamang","Crowned Lemur","South Asian River Dolphin","White fronted Lemur","Patagonian Huemul","Sambirano Mouse Lemur","Montagne D' Ambre Fork marked Lemur","Simmons' Mouse Lemur","Giant Panda","Indian Rhinoceros","Mexican Prairie Dog","Spectral Tarsier","Colombian Night Monkey","Giant Armadillo","Mongoose Lemur","Celebes Crested Macaque","Addax","Golden Bamboo Lemur","Javan Surili","Coimbra Filho's Titi Monkey","Hog Deer","Tana River Red Colobus","Amazonian Manatee","Flat headed Cat","Lowland Anoa","Sumatran Orangutan","Talaud Fruit Bat","North Pacific Right Whale","Lyle's Flying Fox","Sun Bear","Dryas Monkey","Giant Otter","Mountain Anoa","Otter Civet","Silvery Gibbon","Northern Muriqui","Marine Otter","Huon Tree Kangaroo","Pygmy Slow Loris","Rodrigues Flying Fox","African Wild Ass","Golden crowned Sifaka","Proboscis Monkey","Greater Mascarene Flying Fox","Javan Rhinoceros","Gaur","Grizzled Tree Kangaroo","Red cheeked Gibbon","Lar Gibbon","Marsh Deer","Owl faced Monkey","Grey headed Flying Fox","Collared Brown Lemur","Agile Gibbon","Sulawesi Fruit Bat","Malay Tapir","Indochinese Lutung","Black crowned Central American Squirrel Monkey","Zebra Duiker","New Caledonia Flying Fox","Javan Lutung","Taruca","Sumatran Serow","Ornate Flying Fox","Pig tailed Langur","Tana River Mangabey","Galápagos Fur Seal","Temminck's Flying Fox","Ryukyu Flying Fox","Pileated Gibbon","Atlantic Titi","Blond Titi Monkey","Indian Ocean Humpback Dolphin","Pale Fork marked Lemur","Peruvian Yellow tailed Woolly Monkey","Saola","Red Panda","Silvery brown Tamarin","Phayre's Leaf monkey","Philippine Pangolin","Goeldi's Monkey","Moluccan Flying Fox","Black footed Ferret","Coastal Black handed Titi","Sunda Fruit Bat","Barasingha","Stump tailed Macaque","Dama Gazelle","Thylacine","Ashy Red Colobus","White thighed Colobus","Eld's Deer","Mountain Tapir","Tonkean Macaque","Heck's Macaque","Chacoan Peccary","Australian Snubfin Dolphin","Kloss's Gibbon","François's Langur","Guiña","Lion tailed Macaque","Golden headed Lion Tamarin","San Martin Titi Monkey","Wild Yak","Coquerel's Sifaka","North Moluccan Flying Fox","Seram Flying Fox","Indian Pangolin","Northern White cheeked Gibbon","Bilby","Red Slender Loris","Sanje River Mangabey","Hairy eared Dwarf Lemur","Peruvian Woolly Monkey","Sloth Bear","Horsfield's Tarsier","Perrier's Sifaka","Vanauatu Flying Fox","Sambirano Fork marked Lemur","Aldabra Flying fox","Goodfellow's Tree Kangaroo","Jentink's Duiker","Vaquita","Northern Pudu","Borneo Bay Cat","Black bearded Flying Fox","Seal's Sportive Lemur","Alaotra Reed Lemur","Nilgiri Langur","Southern River Otter","Darwin's Fox","Northern Giant Mouse Lemur","Eastern Long beaked Echidna","Kouprey","Slender horned Gazelle","Sanford's Brown Lemur","Sambirano Lesser Bamboo Lemur","Sumatran Surili","Kipunji","Red handed Howler Monkey","Black Lion Tamarin","Black Crested Gibbon","Buffy tufted ear Marmoset","Golden mantled Tree Kangaroo","Smooth coated Otter","Tamaraw","Toque Macaque","Brazilian Bare faced Tamarin","Hairy nosed Otter","Van der Decken's Sifaka","Pygmy Three toed Sloth","Pemba Flying Fox","Ka'apor Capuchin","Brown headed Spider Monkey","Guam Flying Fox","Temotu Flying Fox","Black faced Lion Tamarin","Atlantic Humpbacked Dolphin","Forest Musk Deer","White cheeked Spider Monkey","Purple faced Langur","Northern Pig tailed Macaque","Doria's Tree Kangaroo","Dhole","Andean Night Monkey","Banded Hare Wallaby","Chinese Mountain Cat","Sir David's Long beaked Echidna","Bengal Slow Loris","Southern Giant Clam","Giant Clam","Club Naiad","Dromedary Naiad","Madagascan Mantella","Golden Mantella","Blue legged Mantella","Green Golden Frog","Black eared Mantella","Eastern Golden Frog","Malagasy rainbow frog","Cowan's Mantella","Bernhard's Mantella","Haraldmeier's Mantella","Giant Slippery Frog","Red tailed Knobby Newt","Cerro Campana Stubfoot Toad","Mount Nimba Viviparous Toad","Granular Poison Frog","Sword tailed Newt","Golden Poison Frog","Axolotl","Epipedobates tricolor","Laotriton laoensis","Silverstone's Poison Frog","Golfodulcean Poison Frog","Cainarachi Poison Frog","Tylototriton vietnamensis","Summers' Poison Frog","Hynobius amjiensis","Minute Tree Toad","Chinese Giant Salamander","Sky Blue Poison Dart Frog","Kihansi Spray Toad","Nectophrynoides cryptus","Millepora striata"],"range":["#E4B60080","#DF831880","#C1552080","#92393780","#5C3C7C80","#3D528480","#356B8E80","#387B8580","#3C775980","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#E4B600CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#DF8318CC","#C15520CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#923937CC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#5C3C7CCC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#3D5284CC","#356B8ECC","#356B8ECC","#356B8ECC","#356B8ECC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#387B85CC","#3C7759CC"]},"valueField":"size","percent":true,"count":true,"explanation":null,"breadcrumb":[],"legend":false,"sortFunction":null,"sumNodes":true}},"evals":[],"jsHooks":{"render":[{"code":"\n    function(el, x) {\n    $('.sunburst-chart').css('width', 'initial');\n    $('.sunburst-chart').css('height', 'initial');\n    $('.sunburst-chart').css('left', '50%');\n    $('.sunburst-chart').css('transform', 'translate(-50%, 0)');\n    }\n    ","data":null}]}}</script>

<!--/html_preserve-->

## Tracing Supply and Demand

### The Algorithm

### Following the Demand

### Finding the Supply

## Summary of Results

## Limitations

  - Wild
  - Unidentified Species

## References
