<!-- README.md is generated from README.Rmd. Please edit that file -->

# PAFTOL\_magnoliids

<!-- badges: start -->

[![R CMD
Check](https://github.com/ajhelmstetter/PAFTOL_magnoliids/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ajhelmstetter/PAFTOL_magnoliids/actions/workflows/R-CMD-check.yaml)
[![License: GPL (&gt;=
2)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%202%29-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)
[![Dependencies](https://img.shields.io/badge/dependencies-16/144-red?style=flat)](#)
<!-- badges: end -->

Research Compendium of the project: PAFTOL Magnoliids

### Name change for some files

<table>
<caption>Files that were renamed after naming error (suggested by O. Maurin).</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Old R1 filename</th>
<th style="text-align: left;">New R1 filename</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">PAFTOL_014093_1.fastq.gz</td>
<td style="text-align: left;">PAFTOL_014095_1.fastq.gz</td>
</tr>
<tr class="even">
<td style="text-align: left;">PAFTOL_014095_1.fastq.gz</td>
<td style="text-align: left;">PAFTOL_014097_1.fastq.gz</td>
</tr>
<tr class="odd">
<td style="text-align: left;">PAFTOL_014097_1.fastq.gz</td>
<td style="text-align: left;">PAFTOL_014099_1.fastq.gz</td>
</tr>
</tbody>
</table>

Files that were renamed after naming error (suggested by O. Maurin).

### Taxa with only 10 loci or fewer recovered

<table>
<caption>Samples where we recovered a low number of exons using hybpiper</caption>
<thead>
<tr class="header">
<th style="text-align: left;">sample</th>
<th style="text-align: left;">no. exons recovered</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Gbel</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="even">
<td style="text-align: left;">P_016915</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="odd">
<td style="text-align: left;">P4172</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="even">
<td style="text-align: left;">S47</td>
<td style="text-align: left;">0</td>
</tr>
<tr class="odd">
<td style="text-align: left;">S24</td>
<td style="text-align: left;">3</td>
</tr>
<tr class="even">
<td style="text-align: left;">S39</td>
<td style="text-align: left;">3</td>
</tr>
<tr class="odd">
<td style="text-align: left;">P0110D</td>
<td style="text-align: left;">4</td>
</tr>
<tr class="even">
<td style="text-align: left;">P_014097</td>
<td style="text-align: left;">6</td>
</tr>
<tr class="odd">
<td style="text-align: left;">P_011589</td>
<td style="text-align: left;">8</td>
</tr>
<tr class="even">
<td style="text-align: left;">S25</td>
<td style="text-align: left;">9</td>
</tr>
<tr class="odd">
<td style="text-align: left;">GAP_79789</td>
<td style="text-align: left;">10</td>
</tr>
<tr class="even">
<td style="text-align: left;">P_007864</td>
<td style="text-align: left;">10</td>
</tr>
</tbody>
</table>

Samples where we recovered a low number of exons using hybpiper

### Relevant programs

-   Treeshrink: <https://github.com/uym2/TreeShrink>
-   phyparts: <https://bitbucket.org/blackrim/phyparts/wiki/Home>
-   discovista: <https://github.com/esayyari/DiscoVista>
-   astral-pro: <https://github.com/chaoszhang/A-pro>
-   c++ astral:
    <https://github.com/chaoszhang/ASTER/tree/548fd31493397ec44f5c8e3452c689e547b3e271>
-   ggtree and plotly for interactive paralogs:
    <https://yulab-smu.top/treedata-book/related-tools.html#plotly>

### Relevant papers

-   Yan Z., Du P., Hahn M.W., Nakhleh L. 2020. Species tree inference
    under the multispecies coalescent on data with paralogs is accurate.
    bioRxiv:498378.

### To do:

-   Compare to PAFTOL 353 paper
-   Send list of species to everyone, so far (that work)
-   Alignments failed? CHECK: 5841, 5899, 6363
-   Keeping the whole tree vs removing those taxa from alignment with
    poor exon length recovery
    -   Use the test\_seq\_lengths.txt to remove individuals in
        supercontigs
-   Remove from Hybpiper and rerun retrieve: *Cananga*

## Meetings

### Meeting 21/04/21

-   send 1kp needed to kevin
-   compare to paftol
-   effect of supermatrix vs coalescent
-   If we too stringent, we may biasing the sampling towards taxa that
    are closer to the sequences represented in the bait kit
-   gradient plot but with genera instead of samples

### How to cite

Please cite this compendium as:

> **{{ CITATION TO BE ADDED UPON PUBLICATION }}**

### Content

This repository is structured as follow:

-   [`data/`](https://github.com/ajhelmstetter/PAFTOL_magnoliids/tree/master/data):
    contains all raw data required to perform analyses **(Hidden until
    publication)**

-   [`rscripts/`](https://github.com/ajhelmstetter/PAFTOL_magnoliids/tree/master/rscripts/):
    contains R scripts to run each step of the workflow

-   [`outputs/`](https://github.com/ajhelmstetter/PAFTOL_magnoliids/tree/master/outputs):
    contains all the results created during the workflow

-   [`figures/`](https://github.com/ajhelmstetter/PAFTOL_magnoliids/tree/master/figures):
    contains all the figures created during the workflow

-   [`paper/`](https://github.com/ajhelmstetter/PAFTOL_magnoliids/tree/master/paper):
    contains all the manuscript and related content (biblio, templates,
    etc.)

-   [`R/`](https://github.com/ajhelmstetter/PAFTOL_magnoliids/tree/master/R):
    contains R functions developed especially for this project

-   [`man/`](https://github.com/ajhelmstetter/PAFTOL_magnoliids/tree/master/man):
    contains help files of R functions

-   [`DESCRIPTION`](https://github.com/ajhelmstetter/PAFTOL_magnoliids/tree/master/DESCRIPTION):
    contains project metadata (author, date, dependencies, etc.)

-   [`make.R`](https://github.com/ajhelmstetter/PAFTOL_magnoliids/tree/master/make.R):
    master R script to run the entire project by calling each R script
    stored in the `rscripts/` folder

### Usage

Clone the repository, open the `.Rproj` file in RStudio and run:

    source("make.R")

### Notes

-   All required packages, listed in the `DESCRIPTION` file, will be
    installed (if necessary)
-   All required packages and R functions will be loaded
-   Some analyses listed in the `make.R` might take time
