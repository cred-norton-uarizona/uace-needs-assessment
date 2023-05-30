# uace-needs-assessment

<!-- badges: start -->
[![connect-publish](https://github.com/cred-norton-uarizona/uace-needs-assessment/actions/workflows/connect-publish.yaml/badge.svg)](https://github.com/cred-norton-uarizona/uace-needs-assessment/actions/workflows/connect-publish.yaml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7931081.svg)](https://doi.org/10.5281/zenodo.7931081)
<!-- badges: end -->

University of Arizona Extension (UACE) undertook a statewide needs assessment process in Fall 2022, led by the Community Research, Evaluation, and Development (CRED) team. The purpose of the needs assessment was to understand community needs and assets across the state as a basis for examining and prioritizing Extension activities to maximize our impact now and into the future. This code produces a `Shiny interactive dashboard` (https://viz.datascience.arizona.edu/uace-needs-assessment/) that will enable users to look at the results by different filters including sociodemographics, knowledge of and experience with Extension, and how much respondents said they knew about content areas.

Learn more about this project here: https://norton.arizona.edu/uace-needs-assessment-2022

## Contributing

This project uses [`renv`](https://rstudio.github.io/renv/articles/renv.html) for package management.
When opening this repo as an RStudio Project for the first time, `renv` should automatically install itself and prompt you to run `renv::restore()` to install all package dependencies.
After installing or upgrading packages, you'll need to run `renv::snapshot()` to record these version in the lockfile.
This ensures that the published version of the Shiny app is using the same version of R packages as the ones you use locally.

To contribute to this project, please create a new branch for your changes and make a pull request.
One easy way to do this from within R is with the `usethis` package and the `pr_*` functions.
`pr_init("branch-name")` begins a new branch locally, `pr_push()` helps you create a new pull request, and after it is merged you can use `pr_finish()` to clean things up.
More about this workflow [here](https://usethis.r-lib.org/articles/pr-functions.html).

**If you'd like to cite this project, please give credit to the Community Research, Evaluaiton and Development (CRED) Team in UArizona's Norton School of Human Ecology.**

**UACE Statewide Needs Assessment 2022**
Gildersleeve, R., Leih, R., deBlois, M., Tanoue, K.H., Avery, D., Dominguez, V. & Walsh, M. (2023). UA Cooperative Extension Statewide Needs Assessment 2022 | Community Research, Evaluation and Development (CRED) Team in the Norton School of Human Ecology. https://norton.cals.arizona.edu/uace-needs-assessment-2022

**If you use this code or want to cite this app, please use this citation.**
Ewinghill, T., Scott, E.R., & Guo, J.. (2023). Code for "University of Arizona Cooperative Extension Needs Assessment 2022 Dashboard" (v1.0.0). Zenodo. https://doi.org/10.5281/zenodo.7931081

## Files in the Repository

Within this repository, there are the following folders:

`.github`
 - workflows (for automatic updates to the app)

`dashboard`
 - labels file has the list of survey items
 - ui, server, global scripts for the Shiny app
 - www folder contains images
 - az counties provides the shapefiles for the map on the demographics page
 - rsconnect folder links to the location of the dashboard on R Studio Connect for UArizona

`data`
  - .gitignore file
  - Arizona_County_Boundaries.geojson
  - labels file has the list of survey items

`notes`
  - Survey for reference and variable names
  - bytopicbarchart
  - bytopicbarchart_plotly
  - demographic_donuts_plotly
  - demographics_page
  - example-map
  - test-map
  - demographic_donuts
  - leaflet-chloropleths
  - sample-size-indicator
  - top20ishbarchart
  - selectize_prototype
    
`renv`
  - library folder
  - staging folder
  - .gitignore
  - activate (R Script)
  - settings.json

`scripts`
  - Rhistory
  - 01-clean-survey-data
  - 02-wrangle-survey-data

Other files include:
`.gitignore`
`.renvignore`
`.Rhistory`
`.Rprofile`
`CITATION.cff`
`LICENSE`
`README`
`renvlock`
`uace-needs-assessment` (R Project)
