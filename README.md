# EGAD: Ultra-fast functional analysis of gene networks

Attempts for functional predictions of uncharacterized genes rely substantially on the quality of experimental data that can highly vary due to biases arising from small sample sizes and the presence of noise. A major challenge lies in identifying such artifacts and separating them from biological meaningful information.

With the EGAD (Extending ‘Guilt-by-Association’ by Degree) package, we present a series of highly efficient tools to calculate functional properties in networks based on the guilt-by-association principle. These allow rapid controlled comparisons and analyses. Two of the core features are: a function prediction algorithm which is fully vectorized (neighbor_voting), allowing network characterization across even thousands of functional groups to be accomplished in minutes in cross-validation and an analytic determination of the optimal prior to guess candidates genes across multiple functional sets (calculate_multifunc, auc_multifunc).

The functions implemented here can be applied to gene networks constructed from a range of data types (e.g., protein-protein interactions, expression, etc) across a subset of species with available functional annotations (e.g., human, mouse, zebrafish, worm, fly and yeast). 

## Installation

### Bioconductor 
The EGAD package has been accepted at [Bioconductor](http://bioconductor.org/). If you have bionconductor installed ('https://www.bioconductor.org/install/'), use the following command below. That will install the appropriate EGAD version. Make sure you have the latest verisons of R and biocLite when trying to install. We've noted some issues with the installation through bioconductor. 
```
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("EGAD")
``` 

### Github 
```{r}
devtools::install_github("sarbal/EGAD/EGAD", build_vignettes = TRUE)
```

### EGAD lite 
The core functions of EGAD can be found in the EGADlite binary file. You can simply download and load this file into your R session. There are no package dependencies, except for needing igraph if you wish to use the extend_network function.    
```{r}
load("EGADlite.RData")
```

## Quick start 
Here is a quick example on how to run the neighbor_voting algorithm on a binary network. 
```{r}
# Load EGAD and the data files 
library(EGAD)
data(biogrid)
data(GO.human)

# Or you can load EGADlite here too:
# load("EGADlite.RData")
# download the data folder into your directory and run
# load("data/biogrid.RData")
# load("data/GO.human.RData")

# Make your gene list and the network 
genelist <- make_genelist(biogrid)
gene_network <- make_gene_network(biogrid,genelist)

# Store your annotation matrix
goterms <- unique(GO.human[,3])
annotations <- make_annotations(GO.human[,c(2,3)],genelist,goterms)

# Run GBA 
GO_groups_voted <- run_GBA(gene_network, annotations)

# neighbor voting AUROCs
auc_GO_nv = GO_groups_voted[[1]][,1]

# node degree AUCs
auc_GO_nd = GO_groups_voted[[1]][,3]
```

## Note 
This tool is *very* memory intensive! We recommend you increase your memory to the max (memory.limit(XXXX) ). 

