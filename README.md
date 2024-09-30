Climate Data Analysis and Mortality Prediction
This repository contains various scripts used for analyzing climate data, calculating extreme heat events, and predicting heat-related mortality. The scripts are organized to perform tasks such as processing historical and future climate data, calculating extreme heat thresholds, and training Distributed Lag Non-linear Models (DLNM) to estimate mortality risk across different age groups.

Table of Contents
Day/Night Classification
Heat Threshold Calculation
Data Preprocessing
Population Data Processing
Hourly History Data Extraction
Future Data Extraction
Population-Weighted Calculation
Projected Weather Span Calculation
Future Data Preprocessing
DLNM Models
Heat Mortality Prediction
Heat-Related Mortality Distinction
Figures
Day/Night Classification
File: Daynight_data_calcuation.R
This script divides day and night based on sunrise and sunset times, converting UTC to local time.

Heat Threshold Calculation
File: heat_threshold_calculation.R
Calculates extreme heat thresholds (90th, 95th, 99th percentiles) using a rolling window (10, 15, 30 days) based on the past 50 years' climate data.

Data Preprocessing
File: Data_preprocess.R
Aggregates hourly data into weekly scales. It classifies extreme heat events by counting the number of extreme heat days or nights (both consecutive and non-consecutive). Mortality data is stratified by age groups (0-15, 16-65, 65+), and combined with climate data for model input.

Population Data Processing
File: popdata_preprocess.R
For the years before 2014, where Level 3 population data is missing, this script estimates Level 3 populations by distributing the NUTS-Level 2 total population (2010-2014) according to 2014’s NUTS-Level 3 ratios.

Hourly History Data Extraction
File: Hourly_history_data_extract.py
Processes historical climate data for the year 2022 using population weighting. Since population data is only available at 10-year intervals, the 2020 population data is used for 2012 climate data weighting.

Future Data Extraction
File: Future_data_extract.py
Handles future climate data extraction and population weighting. Non-integer year climate data (e.g., 2031) is weighted using integer-year population predictions (e.g., 2030 population data).

Population-Weighted Calculation
File: pop_weighted_calculation.py
Calculates population-weighted grid cell data to be used as input in Hourly_history_data_extract.py and Future_data_extract.py.

Projected Weather Span Calculation
File: Projected_weather_span.py
Calculates global average temperature and Humidex, area-weighted by grid size.

Future Data Preprocessing
File: Futuredata_preprocess.R
Aggregates hourly future climate data into weekly scales for further analysis.

DLNM Models
Main Model: DLNM_model_agegroup_ns.R
The primary Distributed Lag Non-linear Model (DLNM) used to generate the key conclusions of the study.

Sensitivity Models: DLNM_model_agegroup_ns_**.R
Additional models used for sensitivity analysis with various parameters.

Window Sensitivity Analysis: sensitivity_analysis_window.R
Conducts sensitivity analysis on the window sizes used to define extreme heat events.

Model Validation
File: Model_validation.R
Performs validation on the DLNM models using various metrics.

Heat Mortality Prediction
Historical Prediction (Humidex): heat_mortality_predicted_history.R
Predicts heat-related mortality for the years 2010-2022 using Humidex.

Historical Prediction (Temperature): heat_mortality_predicted_history_temp.R
Predicts heat-related mortality for the years 2010-2022 using temperature data.

Future Prediction: heat_mortality_predicted_future.R
Predicts heat-related mortality for the years 2023-2100 using Humidex.

Heat-Related Mortality Distinction
File: heat_related_mortality_distinction_moderate&extreme.R
This script distinguishes between moderate and extreme heat-related mortality.

Figures
Figure 1:

Figure_1a-c.R - Generates panels (a-c) for Figure 1 in the manuscript.
Figure_1d.R - Generates panel (d) for Figure 1.
Figure 2: Figure_2.R
Generates Figure 2.

Figure 3: Figure_3.R
Generates Figure 3.
