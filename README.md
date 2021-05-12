# **Exploring the vegetation resilience concept with land surface model scenarios**

The concept of resilience can be helpful in describing the relationship between vegetation and climate, especially in the context of climate change. However, the quantification and characterization of resilience is a challenge, due to the inherent complexity of the concept, as well as difficulty in comparing different ecosystems across the globe. In order to explore the vegetation resilience to drought, we estimated the resilience and related metrics from a series of land surface model (LSM) simulations with altered climate forcing data, focusing on the responses to changing precipitation. These simulations were performed in the semi-arid region of Caatinga biome, northeastern Brazil. Results showed that the quantification of resilience components can be represented as a function between precipitation variation and gross primary productivity (GPP) variation. We compared the resilience components estimated for different vegetation types, which showed differences in the response of vegetation to precipitation variability. The study shows the potential of using LSMs to improve our understanding of the vegetation response to climate change, allowing us to explore disturbance and recovery events that are usually not available in field experiments.

## Summary

  - [Getting Started](#getting-started)
  - [License](#license)
  - [Contact](#contact)

## Getting Started

The codes are organized by numbers in front of the file name, and indicates the order in witch they should be runned:

- 01_precip_analysis_.R
- 02_merge_netcdf.R
- 03_merge_table.R
- 04_calculate_metrics.R
- 05_exploratory_analysis.R
- 06_sample_analysis.R

To run the scripts, it is necessary to download the parent directory of this project in any path of the computer, and then run the R files in the indicated order. The requisites to run the scripts are described in the section below.

### Prerequisites

This is a list of required software and libraries, along with their version at the last time they were tested.

**Software:**

- R (4.0.5)
- Rstudio (1.4.113)*

**Libraries:**

- lubridate (1.7.10)
- kableExtra (1.3.4)
- tidyverse (1.3.1)
  - ggplot2 (3.3.3)
  - tibble (3.1.1)
  - tidyr (1.1.3)
  - readr (1.4.0)
  - purrr (0.3.4)
  - dplyr (1.0.5)
  - stringr (1.4.0)
  - forcats (0.5.1)
- tibbletime (0.1.4)
- fs (1.5.0)
- glue (1.4.2)
- tidync (0.2.4)
- ncdf4 (1.17)
- arrow (4.0.0)
- rlang (0.4.11)
- rstatix (0.7.0)
- cowplot (1.1.1)
- hexbin (1.28.2)
- scales (1.1.1)
- ggridges (0.5.3)

*The use of Rstudio is optional but advised

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE.md](LICENSE.md) file for details

## Contact

Feel free to contact me in case you have any problem, question or suggestion.

- hugo.seixas@alumni.usp.br
- tameirao.hugo@gmail.com

