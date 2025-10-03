# Carbon Emissions Forecasting: Europe & Central Asia

## Project Overview
This project analyzes regional CO2 emissions from waste in Europe and Central Asia from 1981 to 2023 and forecasts emissions for 2023–2024. Using World Bank data, the study explores time series properties, decomposes the series, and applies multiple forecasting models including moving average, simple and Holt’s exponential smoothing, and ARIMA. Additionally, the project examines the relationship between CO2 emissions and national income per capita, highlighting economic-environmental linkages over time.

## Dataset
- Data sourced from the World Bank database for Europe and Central Asia.  
- Two CSV files included in the `Data/` folder:
  - `income.csv` – Adjusted net national income per capita (current US$)  
  - `carbon.csv` – CO2 emissions from waste (Mt CO2e)  
- Time frame: 1981–2023  

## Files in This Repository
- `forecast.R` – R script containing all data preparation, analysis, and forecasting  
- `Data/` – Folder containing the two CSV dataset files (`income.csv` and `carbon.csv`)
- `Final_Report.pdf`  – Written report summarizing the analysis, visualizations, and forecasting results

## Tools & Technologies
- **R 4.4.3** – data cleaning, transformation, analysis, and forecasting  
- **forecast, tseries, ggplot2** – R packages used for time series modeling and visualization  

## Key Analyses
- Time series exploration and decomposition of CO2 emissions  
- Forecasting future emissions using moving average, simple and Holt’s exponential smoothing, and ARIMA  
- Examination of the relationship between CO2 emissions and national income per capita  
- Model evaluation and comparison based on fit and forecast accuracy  

## How to Use
1. Clone or download the repository.  
2. Open `forecast.R` in RStudio to reproduce the analysis and forecasts.  
3. Explore the time series decomposition, model comparisons, forecast outputs, and economic-environmental insights.  

## License
This project is for educational and portfolio purposes. Data sourced from the World Bank database.
