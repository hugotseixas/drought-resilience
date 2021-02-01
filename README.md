# Vegetation Resilience Analysis

This is the compilation of codes used to analyse data to support the development of the paper: **Modelling ecosystem resilience to droughts: assessing the estimation of resilience by multiple simulations of precipitation regime.**

**Abstract:**

In the end of 2011 a drought started in the northeast region of Brazil, this event was prolonged until 2017, characterizing the most severe drought event in the last decades for this region. Recent studies found evidence that changes in the rainfall regime indicates an increase of frequency and severity trend for the future. Droughts are well known for impacting human and natural systems, and can significantly affect the global carbon budget, and more specifically in semi arid regions, controls whether the ecosystem will act as a sink or a source of carbon to the atmosphere. It is of great importance to understand and estimate the magnitude of this impact of extreme events, as droughts, in semi arid regions. The use of spatialized estimates of primary productivity, such as the use of process based models, can support these estimations, however, there are few studies validating these kind of models in extreme events. In this study, we assessed the performance of a land surface model (LSM), the community Noah land surface model with multiparameterization options (NOAH-MP), against estimates derived from eddie flux measurements, during the period from 2011 and 2012, which was the most severe period of the 2011/2012 drought event. Results shows that the model accurately represented the latent and sensible heat in different time scales, while the carbon fluxes were poorly estimated in daily and hourly time scales, but well represented in annual and monthly scales. The model showed to be incapable of representing the interactions between water availability (through precipitation) with carbon fluxes, however, this interaction was well represented for precipitation and latent heat. 

## Summary

  - [Getting Started](#getting-started)
  - [Authors](#authors)
  - [License](#license)
  - [Acknowledgments](#acknowledgments)

## Getting Started

The codes are organized by numbers in front of the file name, and indicates the order in witch they should be runned:

- 0_precip_analysis_.R
- 1_merge_netcdf.R
- 2_merge_data.R
- 3_calculate_metrics.R
- 4_create_nc_file.R
- 5_process_nc_data.R
- 6_create_plots.R
- 7_run_analysis.R
- 8_spatial_animations.R * 

*(this script is just some GIF animations to show the spatial distributions of metrics over time, nothing essential)

It is possible to skipt the first four scripts, and start with the "4_process_nc_data.R", since it will load the .nc file create in the previous file, which is already available here. It is important not to rename any of the files present in this project, otherwise the scripts may not work properly. 

To run the scripts, it is necessary to download the parent directory of this project in any path of the computer, and then run the R files in the indicated order. The requisites to run the scripts are described in the section below.

### Prerequisites

This is a list of required software and libraries, along with their version at the last time they were tested.

**Software:**

- R (4.0 - Arbor Day)
- Rstudio (1.3.959)*

**Libraries:**

- lubridate (1.79)
- kableExtra (1.1.0)
- tidyverse (1.3.0)**
  - ggplot2 (3.3.1)
  - tibble (3.0.1)
  - tidyr (1.1.0)
  - readr (1.3.1)
  - purrr (0.3.4)
  - dplyr (1.0.0)
  - stringr (1.4.0)
  - forcats (0.5.0)
- tibbletime (0.1.4)
- tidync (0.2.4)
- ncdf4 (1.17)

*The use of Rstudio is optional but advised

**If you prefer, you don't need to install the tidyverse library, and otherwise install separately its forming libraries. This shouldn't have an impact on functionality of the code, but it would be necessary to modify all scripts and add each of the tidyverse packages as shown below.

    library(ggplot2)
    library(tibble)
    library(tidyr)
    library(readr)
    library(purrr)
    library(dplyr)
    library(stringr)
    library(forcats)

## Authors

  - **Hugo Tameirão Seixas** - *Wrote the codes* -
    [hugotseixas](https://github.com/hugotseixas)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

The headers of the scripts were based on the template created by Dr. Timothy Farewell, available in his website: 
[https://timfarewell.co.uk/my-r-script-header-template/](https://timfarewell.co.uk/my-r-script-header-template/)

The project documentation was based on Billie Thompson models available on her github page:
[https://github.com/PurpleBooth](https://github.com/PurpleBooth)


## Contact

Feel free to contact me in case you have any problem, question or suggestion.

- hugo.seixas@alumni.usp.br
- tameirao.hugo@gmail.com

