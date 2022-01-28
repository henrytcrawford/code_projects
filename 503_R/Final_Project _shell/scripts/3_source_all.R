# source all data before .rmd to PDF
# Not sure why this wont work, but they can be run individual from the files > scripts
library(here)
source(here:here("scripts","0_data_wrangle.R"))
source(here:here("scripts","1_band_plots.R"))
source(here:here("scripts","1_linear_regression.R"))
source(here:here("scripts","2_non-linear_regression.R"))




       