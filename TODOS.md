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

### Package Collaboration
- Experiment with Merging PR from a forked version of this repo

## Archive 

### Patch to work with R 4.2.2
- ~~Currently GH actions fail see [issue #665](https://github.com/r-lib/actions/issues/655)~~
  * ~~undo all changes made to the .yaml the hot fix of `r-version: '4.2.1'`
    * ~~check-standard.yaml~~
    * ~~pkgdown.yaml~~
    * ~~render-markdown.yaml~~
    * ~~style.yaml~~
    * ~~test-coverage.yaml~~
