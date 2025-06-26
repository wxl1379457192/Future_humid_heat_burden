# Project Title

## Overview

Provide a brief description of your project, its objectives, and the problems it addresses. The latest version is in the ‘main-code-version3’ branch.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Data Processing](#data-processing)
  - [Day-Night Data Calculation](#day-night-data-calculation)
  - [Heat Threshold Calculation](#heat-threshold-calculation)
  - [Data Preprocessing](#data-preprocessing)
- [Population Data Handling](#population-data-handling)
- [Modeling](#modeling)
  - [DLNM Model Training and Sensitivity Analysis](#dlnm-model-training-and-sensitivity-analysis)
  - [Model Validation](#model-validation)
- [Prediction and Analysis](#prediction-and-analysis)
  - [Historical Data Prediction](#historical-data-prediction)
  - [Future Data Prediction](#future-data-prediction)
  - [Heat-Related Mortality Distinction](#heat-related-mortality-distinction)
- [Figures and Visualization](#figures-and-visualization)
- [Contributing](#contributing)
- [License](#license)
## Installation

Detail the steps required to install your project, including any dependencies or prerequisites.

## Usage

Provide instructions on how to use your project, including examples and code snippets if applicable.

## Data Processing

### Daynight Data Calculation

Explain how to calculate day and night durations based on sunrise and sunset times, and how to convert UTC to local time. This can be implemented in R using the `Daynight_data_calculation.R` script.

### Heat Threshold Calculation

Describe the method for computing health-related heat thresholds. Refer to the `Local_heat_thershold.R` script for implementation details.

### Data Preprocessing

Outline the steps to aggregate hourly data into weekly summaries, classify weeks based on the presence of extreme heat days or nights, and merge meteorological data with mortality rates segmented by age groups (0-15, 16-65, 65+). The `Data_preprocess.R` script provides a comprehensive guide for this process.

## Population Data Handling

Address the approach for handling missing Level 3 population data prior to 2014 by allocating total population counts from 2010-2014 NUTS-Level 2 data according to 2014 NUTS-Level 3 proportions. The `popdata_preprocess.R` script offers a detailed methodology.

## Modeling

### DLNM Model Training and Sensitivity Analysis

Discuss the training of the Distributed Lag Non-Linear Model (DLNM) for various age groups, highlighting the use of natural splines. The primary model training code is available in `DLNM_model_agegroup.R`, with additional sensitivity analyses in files with similar prefixes, like "DLNM_model_agegroup_**.R".

### Model Validation

Explain the methods employed to validate the model's performance, referencing the `Model_validation.R` script for specific validation techniques.

## Prediction and Analysis

### Historical Data Prediction

Present the use of Humidex and temperature data to predict heat-related mortality from 2010 to 2022, utilizing scripts like `heat_mortality_predicted_history.R` and `heat_mortality_predicted_history_temp.R`.

### Future Data Prediction with and witout adaptation

Elaborate on forecasting heat-related mortality from 2023 to 2100 under no-adaptation assumption using projected Humidex values, as outlined in `heat_mortality_predicted_future.R`. Also, you can project future heat-related mortality under diverse physiological-socioeconomic adaptation scenarios using 'Future_mortality_projection_withadaptation.R'

### Heat-Related Mortality Distinction

Clarify the criteria for differentiating between moderate and extreme heat-related mortality, with guidance provided in `heat_related_mortality_distinction_moderate&extreme.R`.

## Figures and Visualization

Indicate the scripts used to generate figures included in your manuscript, such as `Figure_1a-c.R`, `Figure_1d.R`, `Figure_2.R`, `Figure_3.R`, and `Figure_5.R`, and describe how to interpret these visualizations.


