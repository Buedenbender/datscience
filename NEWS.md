# datscience 0.3.0

* Set the minimum required R version to `4.1`.
  - Package now uses `|>` "new" R native pipe over the magrittr pipe `%>%`
  - Accordingly exchanged all pipe operators
  - Removed dependency of `magrittr`
  
# datscience 0.2.6

* Improved landing page
* Added the todos section to GH Pages (navbar)
* Changes to `flex_table1()`
  - Hot-Fix for trouble when overall colum is included (messed up headers)
  - Improved overall parsing of the str_formula argument
  - Included additional unit tests and warnings / errors for argument

# datscience 0.2.5

* softfork for `flex_table1()` having the opportunity to showcase an overall column
* Updated all GH Actions to v2 ([link](https://github.com/r-lib/actions/tree/v2/examples))
* Added additional GH Actions / Workflows (render markdown, style code)
* Corrected styling (indentation, ..)
* Updated all dependencies and R to 4.2.1

# datscience 0.2.4

* Improved units tests (code cov) and further implemented CI support
* Optimized referencing (scientific sources)
* Updated the fundamentals for the statistics underlying `flex_table1()`
* Updated badges

# datscience 0.2.3

* Added possibility to maintain old itemnames with `spss_swap()` function
* Added the possibility of multiple group comparisons to `flex_table1()` (recommended by Rémi and Taylor)
* Added the function `pretty_cm()` for plotting of caret::confusionMatrix()
* Added the function `get_ICD_10_cats()` that returns ICD-10 F Diagnoses categories (thanks to Dirk Edelbuettel)
* Added the convenience function `independent_sample_means()` to compare means of 2 or more groups (including check of assumptions).

# datscience 0.2.2

* Removed unnecessary dependencies and replaced them with internal utility functions
* Added the function `flex_table1()`
* Added the function `spss_swap()` to exchange labels / description field of spss with itemnames
* Wrote an article describing `flex_table1()`

# datscience 0.2.1

* Modularized the main file datscience.R into small files for every function
* Implemented basic first unit tests
* Added the news.md for a change log of the package

# datscience 0.1.2

* Added a `NEWS.md` file to track changes to the package.
