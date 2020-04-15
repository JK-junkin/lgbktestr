## FUNDAMENTAL
# usethis::create_package("ho") # create minimal (R/ DESCRIPTION NAMESPACES) pkg
devtools::document()    # create man/ etc.. (inherit from roxygen2::roxygenize)
usethis::use_testthat() # create tests/ etc..
usethis::use_data()     # create data/ and export data (.rda)
usethis::use_data_raw() # create data-raw/ etc..

testthat::test_dir()    # Run all tests (devtools::test wraps this function.)
testthat::test_file("tests/testthat/test-utils.R") # Run a specific test code.
lintr::lint_package()   # inspect and suggest linter errors.

devtools::check()       # build bandle package and scan general problems

# devtools::install()  # Wrapper function of `R CMD INSTALL`
# devtools::build()    # Create bandle pkg from source pkg `R CMD BUILD`
# library(lgbktestr)   # Cannot run only after devtools::load_all().
# devtools::reload()   # Reload.

## Regarding DESCRIPTION
purrr::map(.x = c("devtools", "dplyr", "tidyr", "tibble", "stringr", "purrr",
                  "readxl", "magrittr", "EDAWR", "testthat", "knitr",
                  "rmarkdown"),
           .f = usethis::use_package)
purrr::map(.x = c("usethis"),
           .f = usethis::use_package, "Suggests")
