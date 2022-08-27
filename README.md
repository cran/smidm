
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Smidm - Statistical Modelling for Infectious Disease Management

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![pipeline
status](https://gitlab.itwm.fraunhofer.de/fm-ester/codesu/badges/main/pipeline.svg)](https://gitlab.itwm.fraunhofer.de/fm-ester/codesu/-/commits/main)
[![coverage
report](https://gitlab.itwm.fraunhofer.de/fm-ester/codesu/badges/main/coverage.svg)](https://gitlab.itwm.fraunhofer.de/fm-ester/codesu/-/commits/main)
[![CRAN
status](https://www.r-pkg.org/badges/version/smidm)](https://CRAN.R-project.org/package=smidm)
<!-- badges: end -->

## Overview

Smidm implements statistical models and visualizations to support
decision making by health authorities w.r.t. the COVID-19 pandemic. The
application can be viewed [here](https://ester.fraunhofer.de/).

## Installation

This package can be installed for developers with access to this
repository with this command:

    install.packages(c("devtools", "rmarkdown"))
    devtools::install_git(
      "https://gitlab.cc-asp.fraunhofer.de/ester/smidm.git",
      ref = "main",
      build_vignettes = TRUE
      )

The ref argument can be used to specify which version/branch should be
installed.

## Getting started

Several vignettes have been compiled to illustrate the functionality of
the package. An overview can be displayed via:

    vignette(package = "smidm")

To display an individual vignette, e.g. for the prediction when contacts
of an infected person will start to show symptoms, utilize the following
command:

    vignette(topic = "contacts", package = "smidm")

## Conventions

As style guide for this project the [tidyverse
style](https://style.tidyverse.org/) guide is used.

Version numbers of the package are given by the [Semantic
Versioning](https://semver.org).

The default branch is **main**. For adding new features, you need to
create a new branch. On the main branch is no pushing, only merging.

## Development and research

The research project was funded from 15.05.2020 - 14.12.2020 within the
Fraunhofer Anti-Corona Program and from 01.07.2021 - 30.06.2022 within
the program Prevention and Care of Epidemic Infections with Innovative
Medical Technology by the Federal Ministry of Education and Research.

In addition to the R-package a web application was built, which is
available at <https://ester.fraunhofer.de/>.

## Authors and contact

The project was developed by [Fraunhofer Institute for Industrial
Mathematics ITWM](https://www.itwm.fraunhofer.de/en.html), [Fraunhofer
Institute for Digital Medicine MEVIS](https://www.mevis.fraunhofer.de/)
and [Leibniz Institute for Prevention Research and Epidemiology
BIPS](https://www.bips-institut.de/en/home.html) in cooperation with the
health authority Berlin-Reinickendorf.

If you have any questions, feedback, issues/PR, you can contact us via
<ester-info@itwm.fraunhofer.de>.

For further contact information, please visit the website
<https://www.itwm.fraunhofer.de/en/departments/fm/data-science.html>.

## License

Licensed under the [BSD 3-Clause
License](https://opensource.org/licenses/BSD-3-Clause).
