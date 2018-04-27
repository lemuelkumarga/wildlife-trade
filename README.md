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

    ## Base Packages: stats, graphics, grDevices, utils, datasets, base
    ## Attached Packages: bindrcpp, DiagrammeR, tidyr, pander, ggplot2, dplyr, knitr

## About the Data

We will be using wildlife trade data from
<a href="https://www.cites.org/" target="_blank">CITES</a> for the
period of 2001 to 2015, with the following caveats:

  - 2016 and 2017 were excluded due to data lag in the year of analysis
    (2018). (See
    <a href="https://trade.cites.org/cites_trade_guidelines/en-CITES_Trade_Database_Guide.pdf" target="_blank">Section
    1.2.2 of the guide</a> for more details).
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
    between two countries for a particular species at a particular unit.
    This is contrary to popular belief that each row corresponds to one
    shipment. (See Section 3.1 for more details)
2.  Not all animals in the data are endangered. For example, the
    <a href="https://en.wikipedia.org/wiki/White-tailed_eagle" target="_blank">white-tailed
    eagle (Haliaeetus albicilla)</a> is specified as
    <span class="hl">Least Concern</span> on
    <a href="http://www.iucnredlist.org/details/22695137/0" target="_blank">IUCN
    Red List</a>.
3.  Terms are <span class="hl">heterogenous</span>. For examples, some
    quantities correspond to bodies, while others correspond to
    feathers.
4.  Units are also <span class="hl">varied</span>. Quantities can be
    quoted as distinct counts (i.e. blank Unit), or in terms of
    weight/volume/qualitative units.

As can be seen, some pre-processing would be required before our
analysis can proceed. In particular, (3) and (4) need to be standardized
to allow comparison across species.

## Pre-Processing

### Rescoping the Species

#### Animals Only

In the data, taxonomies with blank <span class="hl">Class</span>
correspond to plants. For the purposes of this analysis, we will exclude
non-animals from the dataset.

``` r
pre_clean <- nrow(dataset)

dataset <- dataset %>%
           filter(Class != "")

post_clean <- nrow(dataset)
  
cat(sprintf("%i rows removed (%.0f%% of total)",pre_clean - post_clean,(pre_clean - post_clean)/pre_clean * 100))
```

    ## 31068 rows removed (7% of total)

#### Endangered Only

Endangered status of species are located in
<a href="http://www.iucnredlist.org/" target="_blank">IUCN Red List
database</a>. Due to licensing restrictions, the data could not be
uploaded for public view. However, one is free to create an account and
download the csv from
<a href="http://www.iucnredlist.org/search/saved?id=90695" target="_blank">here</a>.

Using the IUCN data, we can subsequently filter non-endangered species
out of the CITES dataset.

``` r
pre_clean <- nrow(dataset)

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

post_clean <- nrow(dataset)

cat(sprintf("%i rows removed (%.0f%% of total)",pre_clean - post_clean,(pre_clean - post_clean)/pre_clean * 100))
```

    ## 318189 rows removed (75% of total)

### Standardizing the Terms

After restricting the dataset to only endangered species, we need to
standardize all the terms into universal <span class="hl">trading
units</span>. Ideally, 1 trading unit should represent 1 animal.

Thse are the terms left to be standardized:

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

The first thing we notice from the above text is that not all of the
scientific units are SI (examples include <span class="hl">cm, g and
litres</span>). Let us therefore first standardize all units to SI:

``` r
pre_clean <- nrow(dataset %>% group_by(Term, Unit) %>% summarise())

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

post_clean <- nrow(dataset %>% group_by(Term, ConvertedUnit) %>% summarise())

cat(sprintf("%i term-unit pairs removed (%.0f%% of total)",pre_clean - post_clean,(pre_clean - post_clean)/pre_clean * 100))
```

    ## 62 term-unit pairs removed (28% of total)

#### 1 Term, 1 Unit

Another problem to rectify is that 1 term can be recorded in different
units. Consider the term <span class="hl">bodies</span>, which were
recorded in counts, kilograms and even metres:

``` r
term_unit_counts <- dataset %>%
                    mutate(Unit = ConvertedUnit) %>%
                    group_by(Term,Unit) %>%
                    summarise(Records = n(),
                              Quantity = sum(ConvertedQty))
  
output_tbl <- term_unit_counts %>%
              filter(Term == "bodies")

pander(output_tbl)
```

| Term   | Unit | Records | Quantity |
| :----- | :--- | :-----: | :------: |
| bodies |      |  1604   | 5487551  |
| bodies | kg   |   360   |  334071  |
| bodies | m    |    1    |   0.1    |

Ideally, we would be able to convert kilograms of bodies into actual
count by identifying each species’ weight. However, such methods are
time-intensive, and fall outside the scope of this analysis. Therefore,
we need to propose a conversion rule that is more manageable and yet
still relatively accurate.

In order to ensure that the unit for each term is standardized, we will
first choose the target unit for each term. This can be done by
identifying the unit whose records (i.e. rows) are highest for each
term:

``` r
target_unit <- term_unit_counts %>%
               ungroup() %>%
               group_by(Term) %>%
               mutate(r = rank(desc(Records), ties.method = 'first')) %>%
               summarise(
                 NumberOfUnits = n(),
                 TargetUnit = max(ifelse(r == 1, Unit, NA), na.rm=TRUE),
                 Records = max(ifelse(r == 1,Records,NA), na.rm=TRUE)
               )

output_str <- "Terms [with their Target Unit]:\n"
terms_w_target_unit <- target_unit %>%
                       mutate(outputString = paste0(Term, " [", ifelse(TargetUnit == "","count",TargetUnit), "]"))

output_str <- output_str %>%
              paste0(paste0(terms_w_target_unit$outputString,collapse=", "))
  
cat(output_str)
```

    ## Terms [with their Target Unit]:
    ## baleen [count], bodies [count], bone carvings [count], bone pieces [count], bones [count], calipee [count], carapaces [count], carvings [count], caviar [kg], claws [count], cloth [count], coral sand [kg], cosmetics [kg], derivatives [count], dried plants [count], ears [count], eggs [kg], eggs (live) [count], extract [kg], feathers [count], feet [count], fingerlings [kg], fins [kg], fur products (large) [count], gall [count], gall bladders [count], garments [count], genitalia [count], hair [count], hair products [count], horn carvings [count], horn pieces [count], horns [count], ivory carvings [count], ivory pieces [count], ivory scraps [count], jewellery [count], jewellery - ivory  [count], leather items [count], leather products (large) [count], leather products (small) [count], live [count], meat [kg], medicine [kg], musk [kg], oil [count], piano keys [count], plates [count], powder [kg], raw corals [count], rug [count], scales [count], sets of piano keys [count], shells [count], shoes [count], sides [count], skeletons [count], skin pieces [count], skins [count], skin scraps [count], skulls [count], specimens [count], swim bladders [kg], tails [count], teeth [count], trophies [count], trunk [count], tusks [count], unspecified [count], venom [kg], wax [kg]

The tricky part comes in when we attempt to convert a quantity from a
non-target unit to the target unit. During this step, we will assume
that, <span class="hl">for each species, the median quantity of each
term-unit pair corresponds to the same number of live animals</span>. In
other words, if the median quantity of elephant bodies is 2, and the
median quantity of elephant bodies in kgs is 14,000 kg, then the 2
elephant bodies are equivalent to 14,000 kgs.

With this assumption, we can convert a quantity from a non-target unit
to a target unit using the following equation:

\[ q_{t} = \frac{q_{nt}}{m_{nt}} \times m_{t} \]

where <br> \(q_{t}\) corresponds to the quantity in target unit, <br>
\(q_{nt}\) corresponds to the quantity in non-target unit, <br>
\(m_{nt}\) corresponds to the median quantity of the non-target unit
(per record), and <br> \(m_{t}\) corresponds to the median quantity of
the target unit (per record).

With the above formalization, the conversion challenge gets reduced to
finding the median quantity for a particular species, term and unit
triplet.

##### The Roll-Up Median Approach

One potential challenge is that we do not have many data points for each
species, term and unit triplet. To illustrate, consider the African bush
elephant (Loxodonta africana) trades that were recorded in non-target
units:

``` r
output_tbl <- dataset %>% 
              filter(Taxon == "Loxodonta africana") %>%
              left_join(target_unit, by=c("Term"="Term","ConvertedUnit" = "TargetUnit")) %>%
              filter(is.na(NumberOfUnits)) %>%
              mutate(Unit = ConvertedUnit) %>%
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

We see that there is only a <span class="hl">single</span> data point
for Loxodonta africana trades whose term and unit are “bodies” and “kg”.
Such minute sample size is not sufficient to calculate the median
quantity. Therefore, to obtain larger sample sizes, a roll-up approach
is
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
    Class_T [label = 'Does c (the Class of o)\nhave >=10 records\nfor the term t and unit u?']
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

<div id="htmlwidget-9b1770f622839da4a6c0" class="grViz html-widget" style="width:672px;height:700px;">

</div>

<script type="application/json" data-for="htmlwidget-9b1770f622839da4a6c0">{"x":{"diagram":"\n  digraph RollUp {\n\n    # Default Specs\n    graph [compound = true, nodesep = .5, ranksep = .25]\n    node [fontname = \"Source Sans Pro\", fontsize = 14, fontcolor = \"#ffffff\", penwidth=0, color=\"#424242BF\", style=filled]\n    edge [fontname = \"Source Sans Pro\", fontcolor = \"#424242BF\", color=\"#424242BF\"]\n    \n    # Input Specs\n    Inp [fillcolor = \"#424242BF\", label = \"(Species s, Term t, Unit u)\", shape = rectangle]\n\n    # Conclude Specs\n    node [shape = oval]\n    Species_Y [label = \"Use the median quantity\nof Species s, Term t and Unit u.\", fillcolor = \"#335A87\"]\n    Genus_Y [label = \"Use the median quantity\nof Genus g, Term t and Unit u.\", fillcolor = \"#3C7759\"]\n    Family_Y [label = \"Use the median quantity\nof Family f, Term t and Unit u.\", fillcolor = \"#A43820\"]\n    Order_Y [label = \"Use the median quantity\nof Order o, Term t and Unit u.\", fillcolor = \"#5C3C7C\"]\n    Class_Y [label = \"Use the median quantity\nof Class c, Term t and Unit u.\", fillcolor = \"#377D95\"]\n    Kingdom_Y [label = \"Use the median quantity\nof Term t and Unit u (across all).\", fillcolor = \"#424242\"]\n\n    # Trigger Specs\n    node [shape = diamond, fillcolor = \"#ffffff\", fontcolor = \"#424242\", penwidth = 1]\n    Species_T [label = \"Does the Species s\nhave >=10 records\nfor the term t and unit u?\"]\n    { rank = same; Species_T, Species_Y }\n    Genus_T [label = \"Does g (the Genus of s)\nhave >=10 records\nfor the term t and unit u?\"]\n    { rank = same; Genus_T, Genus_Y }\n    Family_T [label = \"Does f (the Family of g)\nhave >=10 records\nfor the term t and unit u?\"]\n    { rank = same; Family_T, Family_Y }\n    Order_T [label = \"Does o (the Order of f)\nhave >=10 records\nfor the term t and unit u?\"]\n    { rank = same; Order_T, Order_Y }\n    Class_T [label = \"Does c (the Class of o)\nhave >=10 records\nfor the term t and unit u?\"]\n    { rank = same; Class_T, Class_Y }\n\n    # Yes edges\n    Inp -> Species_T\n    edge [arrowhead = \"box\", label = \"Yes\"]\n    Species_T -> Species_Y\n    Genus_T -> Genus_Y\n    Family_T -> Family_Y\n    Order_T -> Order_Y\n    Class_T -> Class_Y\n    \n    # No edges\n    edge [label = \"     No\"]\n    Species_T -> Genus_T\n    Genus_T -> Family_T\n    Family_T -> Order_T\n    Order_T -> Class_T\n    Class_T -> Kingdom_Y\n\n  }\n","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

To sum up the flowchart above, for each species, we first attempt to
count the number of records under the given term and unit. If the
records are insufficient, we then “roll-up” and attempt to find the
record count for the genus associated with the species. This process
continues until we obtain a sufficient number of records (in this case
10) to calculate the median.

Using the following process, we are able to convert all the terms to
their respective standardized
units.

``` r
pre_clean <- nrow(dataset %>% group_by(Term, ConvertedUnit) %>% summarise())

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
        Median = ifelse(Records < 10 & !is.na(ProposedRecord),
                        ProposedMedian, Median),
        Records = ifelse(Records < 10 & !is.na(ProposedRecord),
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

post_clean <- nrow(dataset %>% group_by(Term, ConvertedUnit) %>% summarise())

cat(sprintf("%i term-unit pairs removed (%.0f%% of total)",pre_clean - post_clean,(pre_clean - post_clean)/pre_clean * 100))
```

    ## 90 term-unit pairs removed (56% of total)

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
