# gRaphene

Tools for analysis of Raman spectroscopy data, mainly as exported from Renishaw Wire software.

## Installation

The package can be installed using the `devtools` package. I also recommend installing the `tidyverse`, as the package relies heavily on functions from `dplyr`, `tidyr` and `ggplot2`. 

```r
install.packages("tidyverse")
install.packages("devtools")
devtools::install_github("emilbp/gRaphene")
```

## Basic use
See the vignette "curve_fits" for examples of basic use.

```r
library(gRaphene)
vignette("curve_fits")
```

Note: This is also a learning experience for me in building my first R package, as well as learning to use Github. Feedback is much appreciated.
