# Heatwaves and Home Births: Impact of Extreme Heat on Delivery Choices in India

This repository contains the scripts used to study the effect of extreme heat on home deliveries in India. The following sections describe the data sources used in the study and the scripts needed to replicate the analysis. 

# Data Sources
Raw data for Wet Bulb Globe Temperature (WBGT) and Precipitation can be obtained directly using the links below. Data on place of delivery, date of birth, and other individual level socio-economic and demographic variables can be obtained from the Demographic and Health Survey (DHS) website by creating an account and submitting an application to use the data.

* WBGT-Max: https://zenodo.org/records/8021197
* Mean Precipitation: CHIRPS (https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/netcdf/p05/)
* DHS dataset (https://www.dhsprogram.com/data/available-datasets.cfm)

# Scripts used in the study
All scripts to process and analyze data for this project are contained in the '1-scripts' folder. The following sections provide a brief description of the scripts:

## Scripts to read and process data
### 1.1-DHS-IR-create-vars
This script performs several steps to prepare and process the DHS data for analysis, focusing on creating variables for the Individual Recode dataset. It first identifies and selects relevant variables from different domains, such as meta, birth history, institutional delivery, socioeconomic status, and access-related variables. It then reads the raw data file and converts it into a data table format. It filters the data to include only usual residents, women who had a birth in the last five years, and those with caste information. The data is reshaped from wide to long format to get all births, and various new variables are created, including unique identifiers, birth details, institutional delivery indicators, religion, caste classifications, and maternal age-related variables. The final steps include converting specific variables to factors and saving the processed data to a file for future use. 

## 1.2-munge-spatial-read-datasets
This script processes spatial datasets by first reading geo-coded PSU data from a DHS India shapefile, filtering out invalid geocodes, and selecting relevant variables such as PSU, district name, latitude, and longitude. Next, it retrieves the administrative boundaries for India, adds a 50 km buffer around the boundary, and ensures necessary directories exist for saving output files. Finally, the processed geo-coded PSU data and buffered India boundary are saved as RDS files for further use.

## 2.1-clim-extract-wbgt-data-to-psus
This script links daily temperature data with the geocoded PSU data from the DHS. It reads geocoded PSU data and extracts daily gridded climate data, creating a large dataset where each PSU has one row per day for the number of years of climate data available. The script first points to folders containing raw climate data and loads necessary datasets, including geocoded PSU data and India's administrative boundaries. It then sources a custom function to extract climate data for each PSU and applies this function to obtain maximum temperature and wet-bulb temperature, as well as precipitation data. These datasets are saved and then merged, resulting in a combined dataset that includes daily maximum temperature, wet-bulb temperature, and precipitation for each PSU. Finally, missing precipitation values are removed, and the cleaned dataset is saved for further analysis.

## 2.2-clim-create-vars-wbgt-long-term
This script, titled "Create temperature related variables specific to PSU and week/month," processes a large climate dataset to generate temperature-related variables based on long-term trends. The script begins by loading the necessary functions and datasets, including daily temperature and precipitation data for each PSU. It retains only complete cases and prepares the data by creating basic date-related variables.

Using harmonic quantile regression, the script creates cutoff variables for the 90th, 95th, and 97th percentiles of maximum wet-bulb temperature. Additionally, it calculates these cutoffs based on the day of the year. Long-term means and medians for maximum wet-bulb temperature are also computed by PSU and day of the year.

The processed data is saved to a file, capturing temperature extremes specific to both space and time, facilitating further analysis of climate impacts at the local level.

## 2.3-clim-wbgt-vars-for-paper
This script processes climate data to generate temperature-related variables specific to PSUs (Primary Sampling Units) and time (week/month). The script begins by loading necessary packages and reading in a processed dataset of daily climate variables. It filters the data to include only cases from 2014 onwards. It then creates tercile categories based on long-term mean and median temperatures.

Next, the script identifies extreme heat days using absolute temperature cutoffs (30, 31, and 32 degrees Celsius) and calculates the number of consecutive extreme heat days. It creates heatwave variables for 2, 3, and 5 consecutive days of extreme heat for each temperature cutoff.

The script further identifies extreme heat days using percentile-based cutoffs (90th, 95th, and 97th percentiles) based on the day of the year and harmonic quantile regression approaches. It similarly calculates consecutive extreme heat days and creates heatwave variables for these percentile cutoffs.

Finally, the script saves the processed dataset, which includes all the newly created temperature-related variables, for further analysis.

## 3.1-merge-IR-clim
This script combines temperature-related variables with IR (Individual Recode) data. The script begins by loading necessary datasets, including the IR data and the climate data with temperature variables. It removes the `week_of_year` variable from the IR data as it has already been created in a previous step.

The script then merges the IR and temperature datasets based on PSU and date of birth. It assigns descriptive labels to the variables related to absolute temperature cutoffs, day-of-year percentiles, and harmonic percentiles. After centering the mean precipitation variable, the script saves the final processed dataset as an RDS file, ensuring that variable labels are preserved. 

# Scripts used to generate outputs
## 4.1-outputs-descr
This script merges the dataset containing temperature variables with IR (Individual Recode) data, processes the data for analysis, and creates descriptive tables. It begins by loading the necessary datasets, including the final processed dataset and a function for generating weighted tables.

A directory for output is created if it doesn't already exist. The script defines lists of variables related to socioeconomic status (SES) and various temperature exposures based on absolute values, day-of-year percentiles, and harmonic percentiles. These variables are converted to factors.

A survey object is created to handle the survey design and weights. The script generates descriptive Table 1, which includes SES variables and the dependent variable, and saves it as an Excel file.

For Table 2, three sub-tables are created based on different temperature exposure measures: absolute values, day-of-year percentiles, and harmonic percentiles. Each sub-table is generated using the weighted comparison function and saved as an Excel file. Variable names are interleaved with blanks for better readability.

Finally, the script calculates denominators for the outcome variable and other variables, providing a summary of the dataset. The final processed tables are saved for further analysis.

## 4.2-outputs-run-models-full
This script processes a large dataset to run multiple generalized linear mixed models (GLMMs) in parallel using a 32-core machine. It specifies a combination of potential confounders and various exposure variables related to weather conditions. The models aim to assess the impact of these exposures on a binary outcome, `dv_home_del_fac`.

The script begins by loading necessary packages and datasets. It then defines lists of covariates and exposure variables. The GLMMs are run in parallel, using the `foreach` package to iterate over each exposure variable.

Helper functions are defined to construct and run the models. The script registers a parallel backend to utilize multiple cores, subtracting four cores to ensure system stability. Each exposure variable is modeled with the specified covariates and a random intercept for `psu_fac`.

The outputs of all models are saved as an RDS object for further analysis. The entire process is designed to be efficient, leveraging parallel processing to handle the computational load.

## 4.3-extract-model-summary
This script processes model outputs to generate and save tidy summaries, consolidate coefficients, and extract performance metrics such as ICC and R-squared values.

The script begins by creating a directory for the output if it doesn't already exist and then loads the previously saved model outputs. It extracts tidy outputs for each model using broom.mixed::tidy(), exponentiates the coefficients, and calculates confidence intervals.

Next, the tidy outputs are saved to separate sheets in an Excel workbook. The script consolidates the primary exposure coefficients from each model into a single CSV file.

Finally, the script calculates various performance metrics, including adjusted and unadjusted ICC, conditional and marginal R-squared values, AIC, and BIC, for each model. These metrics are compiled into a data frame and saved as a CSV file.

## 4.4-outputs-run-models-interaction
This script processes the merged dataset generated in `script 3.1` to run multiple generalized linear mixed models (GLMMs) with interaction terms. It begins by loading necessary libraries and the final dataset. 

The script specifies lists of potential covariates, interaction terms, and exposure variables related to weather conditions. It defines the outcome variable and generates a list of formulas for each combination of exposure variable and interaction term, incorporating the specified covariates and a random intercept for `psu_fac`.

Using parallel processing, the script runs the models in parallel across multiple cores. Each model is stored in a list with appropriately shortened names. Finally, the models are saved as an RDS object for further analysis, ensuring efficient handling of the computational load through parallelization.

## 4.5-extract-est-cis-for-interaction
This script processes interaction models to extract and save contrast estimates. It begins by loading necessary libraries and the previously saved interaction models. A directory for output files is created if it doesn't already exist.

The script defines interaction terms and identifies the corresponding models. For each interaction term (rural, caste, religion, wealth, long-term maximum temperature mean, and median), the script constructs contrasts and uses the `glht` function from the `multcomp` package to generate generalized linear hypotheses.

For each interaction term, the script loops through the relevant models, extracts the contrast estimates, and saves the results into separate Excel sheets. This process includes creating specific contrasts for each interaction term, calculating the confidence intervals, exponentiating the estimates, and saving the tidy outputs. The results are organized into Excel files, each containing the contrast estimates for the respective interaction terms.

## 4.6-outputs-fig-interactions
This script processes interaction data to create and save visual plots of contrast estimates. It starts by loading necessary libraries and setting up output directories. A custom function reads multiple sheets from Excel files, combines them, and adds appropriate labels for effect modifiers.

The script reads and processes interaction data for various effect modifiers such as caste, religion, residence, wealth, and long-term mean and median temperatures. It filters and transforms the data, creating new variables for thresholds, types, and durations of temperature exposure. Labels for exposure variables are generated to facilitate understanding.

The script combines all processed data and orders the levels of contrast variables. It then creates separate datasets for absolute and relative temperature thresholds, each based on the day of the year or harmonic analysis. The script uses these datasets to generate plots of the effect modifiers.

Finally, the script saves the generated plots to a specified directory using a custom plotting function. The plots are saved in SVG format, ensuring high-quality visuals for further analysis and presentation.

# Scripts containing functions used in the analysis
## 5.1-function-to-extract-climate-data-for-psus
The `merge_dhs_climate` function integrates climate data with DHS PSU geographic data by processing NetCDF files and extracting relevant climate information for each PSU location. It begins by loading necessary libraries (`terra`, `data.table`, `sf`) and retrieving a list of NetCDF files from the specified path. For each file, the function reads the raster, crops, and masks it to fit the specified region boundary. Climate data is then extracted for each PSU location, combined with PSU data (latitude, longitude, district name), and reshaped from wide to long format. The function also extracts start and end dates from the NetCDF file and generates a sequence of dates based on the specified time units, adding these dates to the data frame. Each processed data frame is added to a list. After processing all files, the list of data frames is combined into a single data frame using `rbindlist`. The final combined data frame, which includes climate data for each PSU location along with corresponding dates, is returned.

## 5.2-function-wtd-comparegroups
The `compareGroups_wtd` function generates weighted comparisons between a dependent variable and a list of independent variables in a given dataset. It first checks if the dependent variable is numeric, stopping execution if it is. The function initializes lists and data frames for storing results and determines the number of levels in the dependent variable. It loops through each independent variable, constructing univariate and bivariate tables, and calculating both unweighted and weighted frequencies.

For categorical independent variables, the function calculates chi-square statistics and p-values, while for numeric variables, it computes means, standard deviations, and p-values from a t-test. The results, including variable labels, frequencies, proportions, means, standard deviations, and statistical tests, are combined into a comprehensive data frame. This data frame is either returned in full or truncated to only include univariate results, based on the specified output type.

## 5.3-function-to-plot-effect-modifiers
The `plot_effect_modifier` function generates a plot that visualizes the effect of an effect modifier on various outcomes. It takes a data frame with columns `exp_label`, `OR`, `CILow`, `CIHigh`, `contrast`, and `effect_modifier`, along with the column name and value of the effect modifier to filter the data. The function filters the data based on the provided effect modifier value, calculates the odds ratios (OR) and confidence intervals (CI), and determines the range for the CI values. It then creates a ggplot with facets for each `exp_label`, displaying the OR and CI for each `contrast`. The plot is styled using the Calibri font and classic theme, with adjustments for readability. The `plot_wrapper` function calls `plot_effect_modifier` for different effect modifiers (Caste, Religion, Residence, Wealth, and Long-term mean temperature tertiles) and arranges the resulting plots into a single composite plot using `ggarrange`, aligning them vertically with labels. This composite plot is then returned for further use or display.

## 5.4-function-for-lycday-and-harmonics
The `lycday` function calculates a modified day of the year that accounts for leap years. For dates in leap years, it returns the usual day of the year. For dates in non-leap years, it adjusts the day of the year by adding a cumulative sum based on the sequence of leap and non-leap years.

The `append_harmonics` function adds harmonic sine and cosine components to a data frame based on the modified day of the year calculated by the `lycday` function. It generates these harmonics for a specified number of harmonics (`n_harmonics`) and appends them as new columns to the data frame. These harmonics are useful for capturing seasonal variations in time series data. The sine and cosine components are calculated using the period of the year (365.25 days) and are named sequentially as `harmonic_s1`, `harmonic_s2`, etc., for sine components, and `harmonic_c1`, `harmonic_c2`, etc., for cosine components.

## 5.5-function-for-parallel-proc-quantile-reg
The `parallel_quantile_regression` function performs quantile regression in parallel across multiple processors for each unique Primary Sampling Unit (PSU) in a data frame. It begins by ensuring the data frame is a data.table and setting up a parallel backend using multiple cores. The function then iterates over each unique PSU, subsets the data for the current PSU, and fits a quantile regression model using the specified formula and quantile.

The fitted values from the quantile regression are assigned back to the subset as a new column, named based on the provided variable name and quantile. Each modified subset is returned and combined into a single data frame. After completing the parallel processing, the function stops the parallel cluster and merges the results back into the main data frame. The final data frame, now containing the fitted values for the quantile regression across all PSUs, is returned.

# Scripts used to generate supplementary tables and figures
## 6.1-suppl-full-models-extract-cl-97
Similar to `script # 4.3' but extracts 97% confidence intervals around point estimates.
## 6.2-suppl-full-models-extract-cl-99
Similar to `script # 4.3' but extracts 99% confidence intervals around point estimates.
## 6.3-suppl-run-models-interaction-remaining
Similar to `script # 4.4' but generates models for all exposures not included in the main paper.
## 6.4-suppl-extract-est-cis-for-interaction
Similar to `script # 4.5' and extracts the point estimates and CIs from the models generated in `script # 6.3'
## 6.5-suppl-fig-interactions
Similar to `script # 4.6' but generates figures corresponding to all exposures not included in the main paper.