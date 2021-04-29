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

## Running ASTRAL post genetrees


    bash ~/scripts/post_genetrees_astral.sh ~/programs/ASTRAL/Astral.5.7.7/Astral/astral.5.7.7.jar astral_50_50

## Comments on initial trees

Bill:

-   Some jumbled Lauraceae/Monimiaceae

Thomas:

-   Cananga is still posing problems, clustering with Endiandra far away
    from its normal position. I guess this sample is probably erroneous
    and should be removed.

EJdL:

-   some strange positions for Lauraceae/Monimiaceae and
    Siparunaceae/Atherospermataceae
-   Some of them I already saw in the tree generated by Alex for the
    Monimiaceae paper
    -   Hortonia, Levieria montana, Austromatthaea elegans, and
        Decarydendron ranomafensis
    -   I believe it is because they had few (or zero) genes recovered
        with the expected length
-   I saw some genera missing in the tree
    -   Matthaea, Pendressia and Monimia are missing for Monimiaceae
    -   Sparattanthelium and Illigera for Hernandiaceae
    -   Gomortega for Gomortegaceae
    -   Chimonanthus, Idiospermum and Calycanthus for Calycanthaceae.

Zacky:

-   The position of Haematodendron is obviously not correct, here as
    sister to Siparuna in both the first and second trees. The 75% tree
    shows it in a polytomy with Myristicaceae, so the family is still
    non-monophyletic. We should perhaps remove this sequence as it seems
    to be erroneous or otherwise problematic. Not sure choosing a
    different threshold will place it any better.
-   Paramyristica is an interesting genus with an unstable position. It
    has never been placed properly before and it seems like that may end
    up being the case here as well. At least arguments of it possibly
    being including back into Myristica seem to now be debatable again.
-   Bicuiba’s position shifts in the 75% tree to become sister to the
    rest of the family, which is incorrect.
    -   other genera also appear to be somewhat ‘forced’ into more
        incorrect positions in the 75% tree
    -   For example, Osteophloeum which at 75% is sister to Virola which
        seems more incorrect than its position in the other two trees. I
        expect it to be sister to Iryanthera (similar to what Herve
        previously found), so I think either of its positions found here
        are questionable.
-   I did not manage to see Doyleanthus - was that not included here?

Herve:

-   Rooting: if there is a way of outputting future trees with
    Chloranthaceae as a clade, that would be great and bring us one step
    closer to publication outputs.
-   We are missing a few entire families that we were supposed to have
    from 1KP an/or PAFTOL
    -   Calycanthaceae (1KP and PAFTOL)
    -   Gomortegaceae (1KP)
-   We are missing most genera of Atherospermataceae (e.g.,
    Atherosperma) and Lauraceae (e.g., Ocotea, and many more)
    -   Said to be sequenced by PAFTOL in sampling documents compiled by
        Olivier.
-   Cananga odorata (PAFTOL), Endiandra glauca (PAFTOL or GAP) &
    Haematodendron glabrum (PAFTOL) are definitely suspicious and I
    wonder if this is a result of poor recovery for these species. Is
    there an easy way to output a summary table with basic stats to help
    us assess overall sequencing quality?
-   Laurales: not sure what is going on with Siparunaceae, looks
    suspicious.
    -   Monimiaceae and Lauraceae totally mixed up: this makes no sense!
    -   This issue did not occur in the preliminary trees sent by Andrew
        in December
    -   Yhere are exactly four taxon names that appear to be swapped
        between the two families.
    -   I suspect Austromatthaea elegans (GAP) is a distinct issue
-   It would be critical to put an explicit species name on Virola sp.
    and Iryanthera sp.
-   We will have to put explicit species names on those Annonaceae taxa
    collected together in Borneo.
-   Append 1KP, PAFTOL, and GAP tags to the taxon names on all our trees
-   Is there any chance you could try a supermatrix analysis with either
    all exons or only those 28% with maximal overlap across orders (from
    Venn diagram)?
-   Start assembling a publication-ready sampling table so we can check
    sample IDs if need be.
    -   With species names, sources, vouchers, contributors, and project
        (1KP, PAFTOL, or GAP) will be extremely useful for us to
        continue the conversation

### To do:

-   Compare to PAFTOL 353 paper
-   Send list of species to everyone, so far (that work)
-   Alignments failed? CHECK: 5841, 5899, 6363
-   Keeping the whole tree vs removing those taxa from alignment with
    poor exon length recovery
    -   Use the test\_seq\_lengths.txt to remove individuals in
        supercontigs
-   Remove from Hybpiper and rerun retrieve: *Cananga*
-   It looks like paralogs aren’t really an issue, so no need for
    astral-pro?

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
