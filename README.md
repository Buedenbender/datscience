
# datscience

<!-- badges: start -->
<!-- badges: end -->


This is a small statistics / datascience utility package.
The content of the package were written by partly by me.
Those I wrote not myself are cited and referenced in the documentation of  
the function (see the man dir, or ? e.g. ?corstars)


## List of Functions available in datscience Package
``` 
 - boxplot_t_test()
 - citation_appendix()
 - cols_rename()
 - colstartsw()
 - corstars()
 - max.ina()
 - my_apa()
 - pretty_scree()
 - get_CIs()
``` 

## Installation

You can install the released version of datscience from [GitHub](https://github.com/Buedenbender/datscience#readme) with:

``` r
install.packages("devtools")
devtools::install_github("Buedenbender/datscience")
```

Plese note, that datscience depends on many usefull packages (e.g. dplyr, ggplot2, ...).

#### Installation Troubleshoot

To install datscience you might need to update some required packages (most prominently e.g. [stringi](https://cran.r-project.org/web/packages/stringi/index.html).
Since R-Version 4.x you probably will need to manually download and install Rtools (and add it to the path) in advance,
to be able to successfully update stringi. A quick guide on RTools is given on the cran website:
https://cran.r-project.org/bin/windows/Rtools/

Further some users might encounter: `(System Error 267 @win/processx.c:1040)` in this case, you can try to install `datscience`  
with the remotes package in standalone mode in a fresh R-sessions (no packages loaded) (see also this [stackoverflow posting](https://stackoverflow.com/q/68400661/7318488)):
``` r
Sys.setenv(R_REMOTES_STANDALONE="true")
remotes::install_github("Buedenbender/datscience")
```

## Example

Here is an example for the function corstars which outputs
a nice correlation table marking sig. correlations with asterisks

``` r
library(datscience)
corstars(mtcars)
```

