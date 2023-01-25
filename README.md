![Graphic showing, for each state in the dataset, whether county-level vote totals followed Benford's law in each election year (based on chi-square goodness of fit, alpha 0.05). Deviations were common for both parties, as is discussed in the paper.](/plots/chisq_summary_8x4.png "Summary graphic")


# Benford’s Law and County-Level Votes in US Presidential Elections
**Purpose:** Assessing adherance of county-level presidential election results to Benford's first digit law in 32 US states, from 1996-2020, to provide a broader context for Biden's "non-Benfordness" in Pennsylvania 2020.

This repository contains code and supplemental graphics for [Groharing and McCune (2022)](https://www.tandfonline.com/doi/abs/10.1080/09332480.2022.2066408?journalCode=ucha20). The full article can additionally be read [here](https://chance.amstat.org/2022/04/benfords-law-votes/).

## Contents
The full dataset of candidate vote totals is available in data/county_data_combined.csv. Results from our hypothesis testing are organized under /tables/. 

The R script used to generate this file from state-level data, and perform the analysis featured in our article, is available in analysis.r. The code is organized into subsections, with notes explaining their purpose and results. Since certain portions of the code are more time consuming, I would recommend selectively running sections of the code in RStudio.

Generated graphics are included under /plots/. Some of these don't appear in the final article--either because they were used for initial exploratory analysis, to save space, or because they concern a tangent not discussed in the paper.

## Packages
In analysis.r, I make frequent use of the pipe operator from [magrittr](https://magrittr.tidyverse.org/) and the various data transformation functions from [dplyr](https://dplyr.tidyverse.org/). The included plots were created using [ggplot2](https://ggplot2.tidyverse.org/) and  palettes from [RColorBrewer](https://cran.r-project.org/web/packages/RColorBrewer/index.html).

You can get magrittr, dplyr, and ggplot2 bundled as part of the larger [TidyVerse](https://www.tidyverse.org/) package.
