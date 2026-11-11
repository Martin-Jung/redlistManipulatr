
# redlistManipulatr

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/matthewlewis896/redlistManipulatr.svg?branch=master)](https://travis-ci.com/matthewlewis896/redlistManipulatr)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/matthewlewis896/redlistManipulatr?branch=master&svg=true)](https://ci.appveyor.com/project/matthewlewis896/redlistManipulatr)
<!-- badges: end -->

`redlistManipulatr` provides functions for retrieving and manipulating IUCN Red List habitat, elevation, threat, and population data.

## Installation

Install the package from GitHub with:

``` r
remotes::install_github("matthewlewis896/redlistManipulatr")
```

## API access

`RL_fetch()`, `RL_threats()`, `RL_sp_list()`, and `RL_population()` use the IUCN Red List API. Set your API key in the `IUCN_REDLIST_KEY` environment variable, for example in your `.Renviron` file, or pass it through the function's `key` argument. API keys are available through the account page at [api.iucnredlist.org](https://api.iucnredlist.org/).

## Package contents

Functions intended for direct use are prefixed with `RL_`. They retrieve species lists and assessment data, recode or subset habitat and season values, check elevation ranges, and reshape or crosswalk habitat data. See the help pages for individual functions or the package vignette, `vignette("redlistManipulatr")`, for a workflow.

The package provides data transcribed from IUCN classifications: `habitats`, `seasons`, and `threats`. It includes code tables for habitat suitability and major importance, plus `hab_conversion_lut` for mapping habitat categories to integer names. See `?habitats`, `?seasons`, and `?threats` for sources and details.

Load the package with:

```r
library(redlistManipulatr)
```
