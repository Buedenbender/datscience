
# datscience

<!-- badges: start -->
<!-- badges: end -->


This is a small statistics / datascience utility package.
Some functions were written by myself.
For other functions I just debugged or wrapped the content
tutorials/guides into functions.


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
``` 

## Installation

You can install the released version of datscience from [GitHub](https://github.com/Buedenbender/datscience#readme) with:

``` r
install.packages("devtools")
devtools::install_github("Buedenbender/datscience")
```

Plese note, that datscience depends on many usefull packages (e.g. dplyr, ggplot2, ...).

#### R-Version 4 and above Troubleshoot

To install datscience you might need to update some required packages (most prominently e.g. [stringi](https://cran.r-project.org/web/packages/stringi/index.html).
Since R-Version 4.x you probably will need to manually download and install Rtools in advance,
to be able to successfully update stringi. A quick guide is given on the cran website:
https://cran.r-project.org/bin/windows/Rtools/



## Example

Here is an example for the function corstars which outputs
a nice correlation table marking sig. correlations with asterisks

``` r
library(datscience)
corstars(mtcars)
```

