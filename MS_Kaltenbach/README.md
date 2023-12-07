# Master Thesis Project

This repository contains the code and resources related to my master's thesis "**A Comparative Study on Physiological Responses to Drought in Wild _Vitis_ Species**"

## Overview

**Context and purpose of the study**:
Crossings of three wild _Vitis_ species are commonly used worldwide as rootstocks in grape production. Disease resistance and vigor are among the most important factors for their selection. With climate change resulting in increasing water limitations, finding rootstocks conferring increased tolerance to drought will be of great importance as well. Therefore, identifying _Vitis_ species with improved drought tolerance, and incorporating them into breeding programs, could contribute to more resilient rootstocks under water-limiting conditions. Furthermore, these species will serve as valuable resources for increasing the genetic variation of the current rootstocks available. We hypothesized that species native to drier habitats would exhibit a superior physiological performance under drought stress.

**Materials and methods**:
The root and canopy physiological characteristics, and the anatomical and biochemical bases of photosynthetic capacity of nine North American wild _Vitis_ species across a wide latitudinal range (New England through Mexico) under two soil moisture treatments, (controlled dry down (target: 20–40% w/w ‘drought’) and maintained irrigated (70–90% w/w ‘control’)), were evaluated using a whole-plant experimental approach. We investigated the links between leaf structural diversity and physiological features that enhance photosynthetic capacity under controlled, non-stressed conditions and whether these relationships are upheld under prolonged water stress. Experiments were performed in a greenhouse under ambient atmospheric conditions using clonal and non-grafted saplings of the nine _Vitis species_. Physiological parameters measured throughout the experiment included midday and predawn leaf and stem water potentials, leaf gas exchange, root and leaf biomass, and spectral measurements. Additionally, X-ray imaging of plant tissues was performed at a single time point mid-experiment, and manual segmentation was used to prepare X-ray images for auto- segmentation using machine learning algorithms. Linear regression models were used to describe the relationships between anatomical and physiological variables, and their associations with biogeoclimatic variables.

**Results**:
Our data shows the impact of drought treatment and indicated differential responses to drought stress across species. Furthermore, structural differences that drive photosynthetic responses were observed. Elucidating canopy traits 
associated with improved performance under drought conditions could facilitate the rapid screening of germplasms to develop drought-tolerant rootstocks in the future. 


## Structure


Example structure:
- `data/`: Contains datasets used in the thesis.
- `scripts/`: Includes the source code for analysis and figures.
- `fig_output/`: Holds the figures generated from the code.
- `data_output/`: Holds the data generated from the code.
- `supp/`: Holds materials used for ACi curves and manual and machine-learning segmentation 

## Code Description

The `scripts/` directory is organized based on the primary traits measured and analyzed in this thesis. Its structure is closely aligned with the figures generated for the thesis. Specifically, for anatomical and physiological parameters, ANOVA was utilized to assess significance, and ggplot was employed to visualize the outcomes. Consequently, each trait folder contains these two scripts for analysis.

**Cleaning/Modification**
- `Cleaning_MK.R`: - extracting relevant columns for this research out of major data set
- `subset_creation.R`: - extracting subset of 9 **Vitis** chosen for this species
- `Combine_Licor_WP_by_dates.R`: - merging data frames and subsetting them based on dates for ANOVA

**Map/Coordinates**
- `coordinates_table.R`: Jupyter Notebook containing model training procedures.
- `map_stacked.R`: R script for statistical analysis.

**Wateruse**
- `cumWU_calculation.R`:  Assess cumulative water use per plant to calculate biomass growth/liter water 
- `SWC_Facet.R`: Plot soil water content per species for treatments

**Waterpotential**
- `WP_Facets.R`: Plot pre-dawn and midday leaf water potential

**Biomass**
- `ANOVA_biomass_final.R`: Calculate ANOVA for biomass traits
- `biomass_facet.R`: Plot biomass traits with results from ANOVA

**Physiological**
- `Anova_beginning.R`: Calculate ANOVA for physiological traits at first measurement date
- `Anova_middle.R`: Calculate ANOVA for physiological traits at middle measurement date
- `Anova_final.R`: Calculate ANOVA for physiological traits at final measurement date

- `Beginning_Facet.R`: Plot physiological traits with results from ANOVA at first measurement date
- `Middle_Facet.R`: Plot physiological traits with results from ANOVA at middle measurement date
- `Final_Facet.R`: Plot physiological traits with results from ANOVA at final measurement date

**Segmentation**
- `Anova_segmentation.R`: Calculate ANOVA for leaf anatomical traits
- `Segmentation_Facet.R`: Plot leaf anatomical traits with results from ANOVA

**Correlation**
- `Correlations_d_c`: Calculate correlations among traits for drought treatment and control treatment individually.
                      Estimated using mean values per species


## Author

This code was written by Miriam Kaltenbach, Master's student in Viticulture and Enology at UC Davis. 

## Acknowledgments

I express my sincere gratitude to Dr. Elisabeth Forrestel, my advisor. Her belief in my capabilities pushed me beyond my expectations, enabling me to complete this thesis.
I would also like to express my appreciation to Dr. Andrew McElrone and Dr. Luis Diaz-Garcia for their valuable advice, encouragement, and constructive critiques, which significantly improved this work.
I am thankful to Dr. Mina Momayyezi’s meticulous mentorship and the dedicated assistance provided by the members of the Forrestel and McElrone labs, who supported me through my research.
I am deeply indebted to my incredible family and friends (back home and in the US) and Dr. Heymann, whose unwavering support has been the bedrock of my accomplishments. This journey and my time at UC Davis would not have been possible without their support.
Finally, I would like to thank the Graduate Group of the Department of Viticulture and Enology and my classmates for making this time special.

