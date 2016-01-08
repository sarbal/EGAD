# EGAD: Ultra-fast functional analysis of gene networks

Attempts for functional predictions of uncharacterized genes rely substantially on the quality of experimental data that can highly vary due to biases arising from small sample sizes and the presence of noise. A major challenge lies in identifying such artifacts and separating them from biological meaningful information.

With the EGAD (Extending ‘Guilt-by-Association’ by Degree) package, we present a series of highly efficient tools to calculate functional properties in networks based on the guilt-by-association principle. These allow rapid controlled comparisons and analyses. Two of the core features are: a function prediction algorithm which is fully vectorized (neighbor_voting), allowing network characterization across even thousands of functional groups to be accomplished in minutes in cross-validation and an analytic determination of the optimal prior to guess candidates genes across multiple functional sets (calculate_multifunc, auc_multifunc).

The functions implemented here can be applied to gene networks constructed from a range of data types (e.g., protein-protein interactions, expression, etc) across a subset of species with available functional annotations (e.g., human, mouse, zebrafish, worm, fly and yeast). 

