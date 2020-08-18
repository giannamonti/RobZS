## Create a new package with RStudio

# Package setup -----------------------------------------------------------

## Use version control
usethis::use_git_config(
  scope = "project",
  user.name = "Gianna Monti",
  user.email = "gianna.monti@unimib.it"
)
usethis::use_git()

# avoid problem with the dev scripts: dev/package-utility.R (this file)
dir.create("dev")
# save this file in `dev` as `setup.R`
usethis::use_build_ignore("dev")

# Fill in the DESCRIPTION file
# rstudioapi::navigateToFile( "DESCRIPTION" )
usethis::use_description(
  list(
    Title = "Robust ZeroSum Regression",
    `Authors@R` = c(person(given = "Gianna",
    											 family = "Monti",
    											 role = c("cre", "aut"),
    											 email = "gianna.monti@unimib.it"),
    								person(given = "Peter",
    											 family = "Filzmoser",
    											 role = c("aut"),
    											 email = "peter.filzmoser@tuwien.ac.at")),
    Description = "R-package for robust elastic net regularized regression with zero sum constraint.",
    URL = "https://github.com/giannamonti/RobZS",
  )
)
usethis::use_lgpl_license( name = "Gianna Monti" )  # You can set another license here

usethis::use_package("parallel")
usethis::use_package("grid")
usethis::use_package("reshape")
usethis::use_package("glmnet")
usethis::use_package("cvTools")
usethis::use_package("robustHD")
usethis::use_package("MASS")
usethis::use_package("zeroSum")
usethis::use_package("mvtnorm")

usethis::use_tidy_description()                           # sort fields and packages



## Common tasks
usethis::use_readme_md( open = FALSE )
# usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )
# usethis::use_news_md( open = FALSE )


## Use tests
usethis::use_testthat()



# Develop -----------------------------------------------------------------

## Add a package
usethis::use_package( "dplyr" )
# add it to ROXYGEN or NAMESPACE

install.packages("roxygen2")

## If you want to use roxygen, enable ROXYGEN in the project.
# Menu: tools > Project options > build tools > generate the documentation with roxygen

file.remove("NAMESPACE")

devtools::document() # to fill NAMESPACE and documentation with ROXYGEN comments
# or roxygen2::roxygenise() # converts roxygen comments to .Rd files.
# or [Ctrl + Shift + D] in RStudio


# Load the package [CTRL + SHIFT + L] or install-and-reload [CTRL + SHIFT + B]

## Check the package for Cran or [CTRL + SHIFT + E]
devtools::check(document = FALSE) # check the package

## Add internal datasets
## If you want to provide data along with your package
usethis::use_data_raw( name = "my_dataset", open = FALSE )

## Tests
## Add one line by test you want to create
usethis::use_test( "hello" )

## Vignette
usethis::use_vignette("ThisTidyPackage")
devtools::build_vignettes()
# Install the package and see it with vignette("ThisTidyPackage")


# Deploy ------------------------------------------------------------------

devtools::missing_s3()

devtools::check()
# rhub::check_for_cran()

