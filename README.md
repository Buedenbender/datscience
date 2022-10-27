
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datscience R-Package

<!-- badges: start -->
<center>

[![R-CMD-check](https://github.com/Buedenbender/datscience/workflows/R-CMD-check/badge.svg)](https://github.com/Buedenbender/datscience/actions)
[![Codecov test
coverage](https://codecov.io/gh/Buedenbender/datscience/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Buedenbender/datscience?branch=main)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg)](https://choosealicense.com/licenses/mit/)
[![Last-changedate](https://img.shields.io/badge/last%20change-2022--10--27-yellowgreen.svg)](commits/main)
[![packageversion](https://img.shields.io/badge/Package%20version-0.2.4-orange.svg?style=flat-square)](commits/main)

</center>
<!-- badges: end -->

The datscience (**dat**aanalysis and **science**) R-package contains
some useful functions frequently required in the process of preparing
data for publication. Most of the current functions provide additional
utility for conducting factor analyses or principal component analyses.

The overall goal was to improve the workflow of data analysis and help
with formatting challenges I encountered in the preparation of
submission to scientific journals. Below are exemplary problems I
frequently encountered in the past: getting the stats from R with the
right format into MS Word, and what this package provides to solve this.

As of March 2022 the new function `datscience::flex_table1()` makes the
creation of the Sociodemograhpic Table 1 (including the statistical
comparisons of subsamples), basically a piece of cake. Take a look at
the new article `vignette("flex_table1")`

### Teaser of `datscience` Functionality

While R provides so many opportunities and power to conduct whatever
analyses one can imagine, I found myself often having difficulties with
the transfer of the analysis or the results from R session into MS Word.

**Example Problem:** Get a *nicely* formatted (in accordance with APA
7th publication manual) correlation table directly into a Word file
(\*.docx).

`datscience` provides with the `apa_corrTable()` function an easy
solution for this task.

``` r
datscience::apa_corrTable(
  df = iris[1:4], summarystats = c("median", "range"),
  filepath = "man/figures/CorrelationTable_iris.docx",
  table_caption = c("Table 1", "Correlation and Descriptive Statistics")
)
```

This creates the path (i.e., directories) `man/figures/` in the current
working directory of R and also the desired word file (see below).

**Screenshot of “CorrelationTable_iris.docx”** ![Screenshot of
apa_corrTable() example](man/figures/README-apa_corrTableExample.png)

**Please Note:** The code for the formatting was inspired and adapted
from the blog post of [Rémi
Thériault](https://rempsyc.remi-theriault.com/articles/table), the
correlations with marked significance from [Dominik Vogel’s
package](https://rdrr.io/github/DominikVogel/vogelR/src/R/output.R)

### 

## Installation

You can install the latest released version of datscience easily,
directly from
[GitHub](https://github.com/Buedenbender/datscience#readme) with:

``` r
# install.packages("devtools")
devtools::install_github("Buedenbender/datscience")
```

**Please Note:** that datscience depends on many useful packages, which
will be installed in the process (e.g. dplyr, ggplot2, …).

#### Installation Troubleshoot

Some users might encounter: `(System Error 267 @win/processx.c:1040)`,
which is due to special characters in the username and the path to the
directories (e.g., `ö` or `é`). In this case, you can try to install
`datscience` with the remotes package in standalone mode in a fresh
R-sessions (no packages loaded) (see below and for reference this
[stackoverflow posting](https://stackoverflow.com/q/68400661/7318488)).

``` r
Sys.setenv(R_REMOTES_STANDALONE="true")
remotes::install_github("Buedenbender/datscience")
```

Further, some users might need to update some of the packages
`datscience` depends on (most prominently
e.g. [stringi](https://cran.r-project.org/web/packages/stringi/index.html)).
Since R-Version 4.x you probably will need to manually download and
install Rtools (and add it to the path) in advance, to be able to
successfully update stringi. A quick guide on RTools is given on the
cran website: <https://cran.r-project.org/bin/windows/Rtools/>

## Examples

The functions I recommed and use the most are
`datscience::format_flextable()` and `datscience::apa_corrTable()`

### apa_corrTable() Function

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
    #> Petal.Length     0.87***    -0.43***              
    #> Petal.Width      0.82***    -0.37***      0.96***
    ```

2.  Appends desired summary stats to the flextable.

3.  Formatting of the `flextable::flextable()` object to APA 7th style,
    by utilizing the `format_flextable()` function. To illustrate the
    function, we here use it to display the first 5 rows of the iris
    data set.

    ``` r
    datscience::format_flextable(flextable::flextable(head(iris, 5)),
      table_caption = c("Table 2", "Illustrating Functionality of format_flextable()")
    )
    ```

    ![FFIE.png](man/figures/README-format_flextableIris.png)

4.  Utilizing the `datscience::save_flextable()` function. This will
    savely (i.e., prohibiting overwrite of files by serializing the
    naming) write the flextable object to a Word (.docx) file

### format_flextable() Function

The flextable package is so versatile and it was exactly what I was
looking for to get nicely formatted tables directly from R(studio) into
Word. The same holds true for the `datscience::format_flextable()`
function from the datscience package. It just applies some repetitive
formatting necessary to convert a flextable to a “publication ready” APA
formatted table.

One example of the flexibility would be to just try to print the factor
loadings from a principal component analysis (PCA, `psych::prinicial`)

**Let’s first create an exemplary
PCA**<sup>\[[1](https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/Harman74.cor),
[2](https://personality-project.org/r/psych/help/principal.html)\]</sup>
**and extract the factor loadings**:  
(Which is also more conveniently packaged in the function
`apa_factorLoadings()` function)

``` r
# Creation of an Example Prinicipal Component Analysis
pacman::p_load(psych, dplyr)
pc <- principal(Harman74.cor$cov, 4, rotate = "varimax")
pc_loadings <- pc %>%
  fa.sort() %>%
  .[["loadings"]] %>%
  round(3) %>%
  unclass() %>%
  as.data.frame() %>%
  mutate(across(
    everything(),
    ~ if_else((. < 0.3), "", as.character(.))
  )) %>%
  bind_cols(
    Communality = pc$communality,
    Uniqueness = pc$uniquenesses,
    Complexity = pc$complexity
  ) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  tibble::rownames_to_column("items")
```

**Formatting these loadings to APA with just one function:**

``` r
formatted_loadings <- datscience::format_flextable(flextable::flextable(pc_loadings),
  table_caption = c("Table 3", "Factor Loadings in Exemplary PCA")
)
formatted_loadings
```

![](man/figures/README-format_flextableExample.png)

## Next Steps for `datscience` R-Package

-   Add a complete vignette for a factor analysis (utilizing `psych` and
    `datscience` package)
-   Beautify / clarify the vignette for bootstrapping confidence
    intervals for observed Eigenvalues in parallel analysis

## Related Work

-   [rempsyc](https://rempsyc.remi-theriault.com/) I highly recommend
    this now available package. It offers great utility for psychologist
    and social scientist getting into R and provides nicely on-point
    tutorials on how to implement the provided functions
    ([one](https://remi-theriault.com/blog_table.html) of which was the
    origin of `datscience::format_flextable()`)

<!-- # Testing Packagedown -->
<!-- ### Additional Remarks -->
<!-- To Be Added Vignette on "Normal" Factor Analysis -->
<!-- #TODO: INCLUDE Reference to Vignettes OR Include Factor Analysis -->
<!-- Minor Changes to Test Github Workflow: Removed Oldrel -->
