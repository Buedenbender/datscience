
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datscience R-Packaage

<!-- badges: start -->
<!-- badges: end -->

The datscience (**dat**aanalysis and **science**) R-package contains
some useful  
functions frequently required in preparing data for publication. Most of
the current functions provide additional utility for conducting factor
analyses or principal component analyses.  
The overall goal was to improve the workflow of data analysis and
formatting challenges I frequently encounter. To illustrate this, I will
present and briefly elaborate one problem I frequently encountered
(getting the stats from R with the right format into MS Word), and what
this package provides to solve this.

### Teaser of `datscience` Functionality

While R provides you with so many opportunities and power to conduct
whatever analyses one can imagine, I found myself often having
difficulties with the transfer of the analysis or the results from R
session into MS Word.

**Example:** Get a *nicely* formatted (in accordance with APA 7th
publication manual) correlation table directly into a Word file
(\*.docx).

datscience provides an easy solution for this task.

``` r
datscience::apa_corrTable(df = iris[1:4], summarystats = c("median","range"),
                          filepath = "man/figures/CorrelationTable_iris.docx",
                          table_caption = c("Table 1","Correlation and Descriptive Statistics")
                          ) 
```

This creates the path (i.e., directories) `man/figures/` in the current
working directory of R and also the desired word file (see below).

**Screenshot of “CorrelationTable\_iris.docx”** ![Screenshot of
apa\_corrTable() example](man/figures/README-apa_corrTableExample.png)

**Please Note:** The code for the formatting was inspired and adapted
from the blog post of [Rémi
Thériault](https://remi-theriault.com/blog_table.html), the correlations
with marked significance from [Dominik Vogel’s
package](https://rdrr.io/github/DominikVogel/vogelR/src/R/output.R)

### 

## Installation

You can install the latest released version of datscience from
[GitHub](https://github.com/Buedenbender/datscience#readme) with:

``` r
# install.packages("devtools")
devtools::install_github("Buedenbender/datscience")
```

Plesae note, that datscience depends on many useful packages, which will
be installed (e.g. dplyr, ggplot2, …).

#### Installation Troubleshoot

To install datscience you might need to update some required packages
(most prominently e.g.
[stringi](https://cran.r-project.org/web/packages/stringi/index.html).
Since R-Version 4.x you probably will need to manually download and
install Rtools (and add it to the path) in advance, to be able to
successfully update stringi. A quick guide on RTools is given on the
cran website: <https://cran.r-project.org/bin/windows/Rtools/>

Further some users might encounter:
`(System Error 267 @win/processx.c:1040)` in this case, you can try to
install `datscience`  
with the remotes package in standalone mode in a fresh R-sessions (no
packages loaded) (see also this [stackoverflow
posting](https://stackoverflow.com/q/68400661/7318488)):

``` r
Sys.setenv(R_REMOTES_STANDALONE="true")
remotes::install_github("Buedenbender/datscience")
```

## Example

The functions I appreciated the most, and frequently use are
`datscience::format_flextable()` and `datscience::apa_corrTable()`

The `datscience::apa_corrTable()` function was already showcased aboved.
This function resolves around three other useful functions from this
package.

1.  Creates the correlation table by calling
    `datscience::corstars()`[<sup>\[1\]</sup>](https://rdrr.io/github/DominikVogel/vogelR/src/R/output.R)).

    ``` r
    datscience::corstars(iris[1:4])
    #>              Sepal.Length Sepal.Width Petal.Length
    #> Sepal.Length                                      
    #> Sepal.Width     -0.12                             
    #> Petal.Length     0.87****   -0.43****             
    #> Petal.Width      0.82****   -0.37****     0.96****
    ```

2.  Appends desired summary stats to, these are generated with
    `psych::describe()`

3.  Formatting of the `flextable::flextable()` object to APA 7th style,
    by utilizing the `format_flextable()` function. To illustrate the
    `format_flextable()` function, we just plot the first 5 rows
    (`head()`) of the iris dataset

    ``` r
    datscience::format_flextable(flextable::flextable(head(iris,5)), 
                                 table_caption = c("Table 2", "Illustrating Functionality of format_flextable()"))
    ```

    ![imagefailedtoload](man/figures/README-unnamed-chunk-4-1.png)

4.  Utilizing the `datscience::save_flextable()` function, which savely
    (i.e., prohibiting overwrite of files by serializing the naming)
    write the flextable object to a Word .docx file \#TODO: INCLUDE
    Reference to Vignettes OR Include Factor Analysis complete Example
