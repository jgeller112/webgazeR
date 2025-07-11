[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://github.com/jgeller112/webgazeR/graphs/commit-activity)
[![DOI](https://zenodo.org/badge/855968611.svg)](https://doi.org/10.5281/zenodo.15831767)


# Webcam Eye-tracking R Package


# WebgazeR

Functions for analyzing webcam eye-tracking data

![webgazeR_hex_sticker](https://github.com/user-attachments/assets/ef82786d-a1a6-4246-a5b2-00eb1312edef)


## Installation

Install the development version from GitHub with:

``` r
# Check if devtools is installed, if not install it
if (!require("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Load devtools
library(remotes)

# Check if ConversationAlign is installed, if not install from GitHub
if (!require("webgazeR", quietly = TRUE)) {
  devtools::install_github("jgeller112/webgazeR")
}

# Load webgazeR
library(webgazeR)
```


A vignette showing how to use the functions contained in here can be found: [https://jgeller112.github.io/webgazeR/](https://jgeller112.github.io/webgazeR/)

# Citation

Please cite this paper when using webgazer:

Geller, J., Prystauka, Y., Colby, S. E., & Drouin, J. R. (in press). Language without borders: A step-by-step guide to analyzing webcam eye-tracking data for L2 research. Research Methods in Applied Lingustics.

```
@article{geller2025language,
  author    = {Geller, Jason and Prystauka, Yana and Colby, Sarah E. and Drouin, Jonathan R.},
  title     = {Language without borders: A step-by-step guide to analyzing webcam eye-tracking data for L2 research},
  journal   = {Research Methods in Applied Linguistics},
  year      = {in press},
  note      = {In press},
}
```

