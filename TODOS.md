## Development Goals
   
  
### Patch to work with R 4.2.2
- Update the R Version (mb with `installr::updateR()`)
- Manually ensure all packages are up to date


### Goals for Version 3.0
- Add linter to CI (GH Actions)
- Fix all lint suggestions
  * Search for an automatization of lint suggestions
- Add a complete vignette for a factor analysis (utilizing `psych` and
  `datscience` package)
- Beautify / clarify the vignette for bootstrapping confidence
  intervals for observed Eigenvalues in parallel analysis
- Exchange all magrittr pipes %>% to the new native R pipe |> 
  * Excluded the dependency (DESCRIPTION) of magrittr
- Reduces number of dependencies
  * is `methods` an necessary import for the  `is()` function
  * maybe `tibble` find a replacement for `rownames_to_column`and `as_tibble`
- Increase the overall number of unit tests

#### Changes To DESCRIPTION / Roxygen / NAMESPACE
- ~~change minimimum required R version to run with the new native pipe ` |> `~~
- Verfiy import format for package development (calling functions from namespace)
  * https://cran.r-project.org/web/packages/import/vignettes/import.html
  * https://r--pkgs-org.translate.goog/Metadata.html?_x_tr_sl=en&_x_tr_tl=de&_x_tr_hl=de&_x_tr_pto=sc
  * https://r-pkgs.org/dependencies.html
  * https://cran.r-project.org/doc/manuals/R-exts.html
  
#### Changes to `flex_table1()`
- Provide alternative methods of feeding the function the desires variables
- Include auto conversion, if `attributes(df$var)` class is of `"haven_labelled"`
  * E.g., with `"haven_labelled" %in% class(df$var)` 
  
  
### Package Collaboration
- Experiment with Merging PR from a forked version of this repo

## Archive 

See also the release notes

### Patch to work with R 4.2.2
- ~~Currently GH actions fail see [issue #665](https://github.com/r-lib/actions/issues/655)~~
  * ~~undo all changes made to the .yaml the hot fix of `r-version: '4.2.1'`~~
    * ~~check-standard.yaml~~
    * ~~pkgdown.yaml~~
    * ~~render-markdown.yaml~~
    * ~~style.yaml~~
    * ~~test-coverage.yaml~~

#### Changes to `flex_table1()`
- ~~Develop unit test for string not starting with a tilde `~`~~ -> added in 2.6
- ~~Release a hotfix for colum test stat now showing a Sample Size insertion~~
