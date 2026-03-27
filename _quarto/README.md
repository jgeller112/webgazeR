# webgazeR <img src="webgazeR.png" align="right" width="150" />

[![R-Universe](https://jgeller112.r-universe.dev/badges/webgazeR)](https://jgeller112.r-universe.dev/webgazeR)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15831768.svg)](https://doi.org/10.5281/zenodo.15831768)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![GitHub last commit](https://img.shields.io/github/last-commit/jgeller112/webgazeR)](https://github.com/jgeller112/webgazeR)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-brightgreen.svg)](https://github.com/jgeller112/webgazeR)

An R package for reading, preprocessing, and analyzing **webcam-based eye-tracking data**. A companion to [gazeR](https://github.com/dmirman/gazer), webgazeR provides a complete pipeline from raw files to analysis-ready data for experiments run on Gorilla, jsPsych, or PsychoPy.

## Installation

```r
# From R-Universe (recommended)
install.packages("webgazeR",
  repos = c("https://jgeller112.r-universe.dev", "https://cloud.r-project.org"))

# From GitHub
# install.packages("remotes")
remotes::install_github("jgeller112/webgazeR")
```

## Supported Platforms

| Platform | Input format | Notes |
|----------|-------------|-------|
| **Gorilla** | `.xlsx` / `.csv` | Filters `type == "prediction"` rows; supports screen index selection |
| **jsPsych** | `.json` / `.csv` / `.tsv` | Handles JSON trials exports and tabular formats with nested gaze arrays |
| **PsychoPy** | `.csv` / `.tsv` | Tabular trial-level data with nested eye sample arrays |

## Documentation

- [Package website](https://jgeller112.github.io/webgazeR/)
- [Introduction vignette](https://jgeller112.github.io/webgazeR/vignettes/webgazeR_vignette.html) -- Full walkthrough with interactive code
- [ISC guide](https://jgeller112.github.io/webgazeR/vignettes/ISC.html) -- Intersubject correlation methods

## Citation

If you use webgazeR in your research, please cite:

> Geller, J., Prystauka, Y., Colby, S. E., & Drouin, J. R. (2025). Language without borders: A step-by-step guide to analyzing webcam eye-tracking data for L2 research. *Research Methods in Applied Linguistics*. https://doi.org/10.1016/j.rmal.2025.100

## License

GPL-3
