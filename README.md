# Weather Data Analysis NYC Central Park (2016) with Stationary AR(p), MA(q), and ARMA(p,q) Models

---

## 📱 **About the Project**

This project analyzes **daily weather data** from **Central Park, New York (2016)** using stationary **AR(p), MA(q), and ARMA(p,q)** models.  
The study evaluates temperature and precipitation variables (max/min/avg temperature, rainfall, snowfall, snow depth), with a focus on capturing both **short-term fluctuations** and **seasonal trends**.

- **Key Finding**: ARMA(1,2) was selected as the best-fit model (lowest AIC & BIC).  
- The model accurately predicts seasonal patterns: rising temperatures in summer and decreasing temperatures in winter.  
- Insights are valuable for **urban planning, infrastructure resilience, and extreme weather preparedness**.  

---

## 📋 Dataset

- **Source**: Central Park Weather Data (2016)  
- **Variables**:  
  - Maximum temperature  
  - Minimum temperature  
  - Average temperature  
  - Rainfall  
  - Snowfall  
  - Snow depth  

---

## 🔎 Methodology

The modeling process followed 4 key stages:

1. **Model Identification**  
   - Examined **ACF** (to identify MA(q)) and **PACF** (to identify AR(p)).  

2. **Stationarity Testing**  
   - **ADF Test** & **KPSS Test** applied.  
   - Differencing performed until stationarity achieved.  

3. **Parameter Estimation**  
   - Used **Maximum Likelihood Estimation (MLE)** and **Least Squares**.  

4. **Model Evaluation & Validation**  
   - Compared models with **AIC & BIC**.  
   - Residual analysis (white noise check using ACF, PACF, Ljung-Box test).  

---

## 🖥 Tools & Libraries

![RStudio](https://img.shields.io/badge/-RStudio-75AADB?style=flat&logo=rstudio&logoColor=white)  
![Forecast](https://img.shields.io/badge/-forecast-FF6F00?style=flat&logo=r&logoColor=white)  
![Time Series](https://img.shields.io/badge/-TimeSeries-1A73E8?style=flat&logo=r&logoColor=white)  
![Data Visualization](https://img.shields.io/badge/-Visualization-FF5733?style=flat&logo=r&logoColor=white)  

---

## 📊 Results

- **Best Model**: **ARMA(1,2)**  
  - Lowest AIC = 1604.122  
  - Lowest BIC = 1617.926  

- **Forecast Performance (ARMA 2,1 with Drift)**  
  - Mean Error (ME): 9.849  
  - Root Mean Squared Error (RMSE): 11.444  
  - Mean Absolute Error (MAE): 9.849  
  - Mean Absolute Percentage Error (MAPE): 10.2%  

✅ Forecast results show that the model is **reliable**, with prediction errors around **10%** of actual values.  

---

## 📈 Visual Insights

- **Boxplot**: NYC daily max temperatures (2016) mostly between 60–80°F, with slight right skew.  
- **Time Series Plot**: Seasonal patterns clearly visible (cool winters, hot summers).  
- **ACF/PACF**: Helped determine AR & MA order.  
- **ROC & Diagnostic Plots**: Residuals confirmed model adequacy (white noise).  
- **Forecast Plot**: ARMA(2,1) successfully predicted upward seasonal trends with 95% confidence intervals.  
