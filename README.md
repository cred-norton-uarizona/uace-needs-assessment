# uace-needs-assessment

<!-- badges: start -->
[![connect-publish](https://github.com/cred-norton-uarizona/uace-needs-assessment/actions/workflows/connect-publish.yaml/badge.svg)](https://github.com/cred-norton-uarizona/uace-needs-assessment/actions/workflows/connect-publish.yaml)
<!-- badges: end -->

The goal of uace-needs-assessment is to ...

## Contributing

This project uses [`renv`](https://rstudio.github.io/renv/articles/renv.html) for package management.
When opening this repo as an RStudio Project for the first time, `renv` should automatically install itself and prompt you to run `renv::restore()` to install all package dependencies.
After installing or upgrading packages, you'll need to run `renv::snapshot()` to record these version in the lockfile.
This ensures that the published version of the Shiny app is using the same version of R packages as the ones you use locally.

To contribute to this project, please create a new branch for your changes and make a pull request.
One easy way to do this from within R is with the `usethis` package and the `pr_*` functions.
`pr_init("branch-name")` begins a new branch locally, `pr_push()` helps you create a new pull request, and after it is merged you can use `pr_finish()` to clean things up.
More about this workflow [here](https://usethis.r-lib.org/articles/pr-functions.html).
