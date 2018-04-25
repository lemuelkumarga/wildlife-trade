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
Locating the Supply and Demand for Endangered Wildlife
================
<span class="meta">Lemuel Kumarga</span>
<span class="meta">May 2018</span>

## Problem Description

Insert Problem Description Here.

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
output_dir <- "output/"

si <- sessionInfo()
base_pkg_str <- paste0("Base Packages: ",paste(si[["basePkgs"]], collapse=", "))
attached_pkg_str <- paste0("Attached Packages: ",paste(names(si[["otherPkgs"]]), collapse=", "))
cat(paste0(base_pkg_str,"\n",attached_pkg_str))
```

    ## Base Packages: stats, graphics, grDevices, utils, datasets, methods, base
    ## Attached Packages: tidyr, pander, ggplot2, dplyr, knitr

## About the Data

<a data-toggle="popover" title="Lorem Ipsum" data-content="Lorem Ipsum is simply dummy text of the printing and typesetting industry. Data obtained from https://www.lipsum.com/.">Lorem
ipsum</a> dolor sit amet, consectetur adipiscing elit. Vivamus sit amet
urna nunc. Ut bibendum sem nibh, lobortis tempor dolor scelerisque ut.
Nunc elit lorem, accumsan in pharetra ac, sodales sit amet arcu. In hac
habitasse platea dictumst. Sed accumsan fringilla purus. Aliquam
tincidunt ultricies sapien, eu pellentesque quam porttitor dignissim.
Cras convallis, ipsum feugiat porttitor tempus, tortor orci fringilla
augue, vel ultrices magna massa varius nibh. Ut gravida posuere dolor,
non tincidunt eros sollicitudin in. Curabitur quis odio condimentum
lectus congue fermentum id eget odio. Proin sagittis, nisl ac imperdiet
ornare, metus risus tempor velit, gravida laoreet lacus purus in metus.
Sed sed cursus dolor. Aliquam lobortis purus eget iaculis interdum.
Maecenas nec eros magna.

Curabitur at urna in urna scelerisque maximus non ut urna. Nulla
pharetra ipsum neque, quis dapibus ex egestas a. Nunc condimentum lectus
et sapien convallis molestie. Suspendisse vel massa fringilla, imperdiet
diam ut, fringilla lacus. Aliquam molestie orci purus, a tristique lacus
malesuada quis. Ut rutrum lacus non vulputate iaculis. Cras tincidunt
risus nisl, lobortis pellentesque purus laoreet in. Ut malesuada erat eu
massa efficitur, condimentum euismod ligula imperdiet. Nunc lorem
tortor, placerat iaculis facilisis vitae, euismod id nisl. Duis
venenatis pharetra arcu sed sagittis. Vivamus eu ex vitae odio eleifend
congue. Proin iaculis imperdiet sapien, et lobortis libero efficitur in.
In ac ex elementum, ornare elit id, lacinia erat.

## Pre-Processing

### Rescoping the Species

### Standardizing the Terms

## Exploration

## Tracing Supply and Demand

### The Algorithm

### Following the Demand

### Finding the Supply

## Summary of Results

## Limitations

## References

## Lorem Ipsum
