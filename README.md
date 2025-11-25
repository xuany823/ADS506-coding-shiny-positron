# ADS506 Assignment 5.1 — Australian Wine Sales Shiny App

This Shiny app was developed for **ADS506: Applied Time Series Analysis** (Assignment 5.1) to explore, model, and forecast **monthly Australian wine sales** across six major varietals. The app provides interactive visualizations and side-by-side comparisons of three forecasting methods: **TSLM**, **ETS**, and **ARIMA**.

---

## Dataset

- **Source**: Australian wine sales dataset compiled by Rob J. Hyndman and colleagues.
- **File**: `data/AustralianWines.csv`
- **Frequency**: Monthly
- **Series**: Six wine varietals (e.g., Dry white, Fortified, Red, Rose, Sparkling, Sweet white)

The CSV is loaded from the local `data/` directory when the app starts.

---

## App Structure & Features

The app consists of four main tabs:

### 1. Visualization

- Select one or more **varietals**.
- Filter the **date range** of interest.
- Choose a **training cutoff year** to define the end of the training period.
- Optionally toggle **points** on the time-series plot.

This tab helps you visually inspect:
- Overall trends in wine sales,
- Seasonal patterns,
- How the training window changes the portion of data used for modeling.

---

### 2. Model Building

This tab focuses on model specification and in-sample performance.

- **Model types**:
  - TSLM (trend + seasonality regression)
  - ETS (Exponential Smoothing)
  - ARIMA (Auto ARIMA)
- **Controls**:
  - Select one or more models.
  - Select one or more varietals.
  - View **Model Specifications** for the chosen models.
  - View **Training Accuracy** (e.g., RMSE, MAE, MAPE) for each model–varietal combination.

This allows users to compare how TSLM, ETS, and ARIMA fit the historical series for different wine types.

---

### 3. Forecast

This tab is designed for forecast comparison and visualization.

- **Forecasting Accuracy**:
  - Compare RMSE, MAE, and MAPE on the **1-year forecast horizon** for TSLM, ETS, and ARIMA.
- **Forecast Visualization**:
  - Plot the last part of the training data plus **12-month-ahead forecasts**.
  - Compare the forecast shapes and prediction intervals across models and varietals.

All models are re-estimated using data up to the **Training cutoff year** selected in the Visualization tab.

---

### 4. About

Provides a short description of:

- The dataset origins,
- The course context (ADS506, Assignment 5.1),
- The main goals of the app (exploration, modeling, and forecasting).

---

## Modeling Details

- The **training period** is dynamically set by the **Training cutoff year** slider.
- For each varietal:
  - TSLM, ETS, and ARIMA models are fitted to data up to the cutoff.
  - 12-step-ahead forecasts are generated.
  - Training and forecast accuracy metrics (RMSE, MAE, MAPE) are computed and displayed.

These results make it easy to compare model performance across both:
- Different **wine varietals**, and  
- Different **modeling approaches** (TSLM vs ETS vs ARIMA).

---

## How to Run the App Locally

1. Clone or download this repository.
2. Make sure the dataset is available at:

   ```text
   data/AustralianWines.csv
