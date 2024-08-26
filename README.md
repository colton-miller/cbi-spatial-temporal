# Spatial alignment and temporal synchrony between field observations and remotely sensed data in empirical studies of burn severity using the Composite Burn Index

## Overview
These data and scripts produce the analysis and visualizations that accompany the paper submitted as:

Spatial alignment and temporal synchrony between field observations and remotely sensed data in empirical studies of burn severity using the Composite Burn Index

Colton W. Miller, Brian J. Harvey, Van R. Kane, Ernesto Alvarado, and L. Monika Moskal

International Journal of Wildland Fire

## System Requirements
- Operating System: Windows, macOS, or Linux
- R version: 4.0.4 or higher
- RStudio (optional, but recommended)

## Installation
The following sections provide detail on how to download and run the scripts needed to produce the tables and visuals included in the publication. 

## Clone the Repository
First, clone the repository to your local machine using Git:

git clone https://github.com/colton-miller/cbi-spatial-temporal.git
cd cbi-spatial-temporal

## Install R and RStudio
- Download and install R from [CRAN](https://cran.r-project.org/).
- (Optional) Install RStudio from [RStudio's website](https://www.rstudio.com/products/rstudio/download/).


### R Dependencies
To ensure compatibility, the following R packages and versions were used:

R version:
|platform       |aarch64-apple-darwin20      |
|---------------|----------------------------|
|arch           |aarch64                     |
|os             |darwin20                    |
|system         |aarch64, darwin20           |
|status         |                            |
|major          |4                           |
|minor          |4.0                         |
|year           |2024                        |
|month          |04                          |
|day            |24                          |
|svn rev        |86474                       |
|language       |R                           |
|version.string |R version 4.4.0 (2024-04-24)|
|nickname       |Puppy Cup                   |


Packages:
- **forcats**: 1.0.0
- **dplyr**: 1.1.4
- **this.path**: 2.4.0
- **readxl**: 1.4.3
- **ggpubr**: 0.6.0
- **ggplot2**: 3.5.1

To install the required R packages, run:

```r
# R
install.packages(c("forcats", "dplyr", "this.path", "readxl", "ggpubr", "ggplot2"))
```

## Running the Project
To run the analysis, open the `/scripts/comparison_results.R` file in RStudio and execute it.

Scripts to produce individual figures can be found in the `/scripts` directory according to their respective caption in the manuscript. 
