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
packages <- c("dplyr","ggplot2","tidyr","pander")
load_or_install.packages(packages)

data_dir <- "data/"
specs_dir <- "specs/"

si <- sessionInfo()
base_pkg_str <- paste0("Base Packages: ",paste(si[["basePkgs"]], collapse=", "))
attached_pkg_str <- paste0("Attached Packages: ",paste(names(si[["otherPkgs"]]), collapse=", "))
cat(paste0(base_pkg_str,"\n",attached_pkg_str))
```

    ## Base Packages: stats, graphics, grDevices, utils, datasets, methods, base
    ## Attached Packages: bindrcpp, tidyr, pander, ggplot2, dplyr, knitr

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
    trade quantities correspond to bodies, while others correspond to
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
uploaded for public use. However, one is free to create an account and
download the csv from
<a href="http://www.iucnredlist.org/search/saved?id=90695" target="_blank">here</a>.

Listed below is an example of the IUCN’s
data:

``` r
# Create an account with IUCN and download the csv from http://www.iucnredlist.org/search/saved?id=90695
# Move the csv into the data folder and rename it as iucn_list.csv

iucn_dataset <- read.csv(paste0(data_dir,"iucn_list.csv"), fileEncoding="latin1")

pander(head(iucn_dataset %>% select(Class, Order, Family, Genus, Species, Common.names..Eng.,Red.List.status),3))
```

| Class      | Order           | Family       | Genus    | Species     | Common.names..Eng. | Red.List.status |
| :--------- | :-------------- | :----------- | :------- | :---------- | :----------------- | :-------------- |
| GASTROPODA | STYLOMMATOPHORA | ENDODONTIDAE | Aaadonta | angaurana   |                    | CR              |
| GASTROPODA | STYLOMMATOPHORA | ENDODONTIDAE | Aaadonta | constricta  |                    | EN              |
| GASTROPODA | STYLOMMATOPHORA | ENDODONTIDAE | Aaadonta | fuscozonata |                    | EN              |

Using the data above, we can subsequently filter non-endangered species
out of the CITES dataset.

``` r
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
  
cat(sprintf("%i rows removed (%.0f%% of total)",pre_clean - post_clean,(pre_clean - post_clean)/pre_clean * 100))
```

    ## 318189 rows removed (75% of total)

### Standardizing the Terms

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
