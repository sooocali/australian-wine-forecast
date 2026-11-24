# Australian wine forecasting explorer

A Shiny application for exploring and comparing monthly Australian wine sales. Select multiple varieties, a date range, a holdout length, forecast horizon, and models. The app displays historical data, forecasts with 95% intervals, a holdout comparison, training and validation accuracy, and fitted model forms. The accuracy table can be downloaded as CSV.

## Run locally

Install R and the packages below. From this folder, run:

```r
install.packages(c('shiny', 'tsibble', 'fable', 'fabletools', 'feasts', 'dplyr', 'tidyr', 'ggplot2', 'distributional'))
shiny::runApp('.')
```

The CSV must remain alongside `app.R`. The app reads it using a relative path. Deploy both files together to a Shiny host such as shinyapps.io; follow the host's current deployment instructions and supply its resulting URL in the submission. This folder does not contain credentials or a deployed URL.

## Data and methodology

The bundled `AustralianWines.csv` is the course file supplied for this assignment. It contains 180 months (January 1980–December 1994) in six series, in thousands of litres. Two Rose observations (July and August 1994) are marked `*` in the source and are linearly interpolated between their neighboring observations. The interpolation is performed before the date filter and split, so for a split between those dates the adjacent later observation contributes to imputation; choose a different split if strict separation is important. Scores covering these two months use imputed values, rather than observed sales.

For each chosen series, the app uses `TSLM(Sales ~ trend() + season())`, automatically selected `ETS(Sales)`, and automatically selected `ARIMA(Sales)`. At least 36 training observations are required. Validation uses the last selected number of months, with forecasts fitted only on the earlier training observations. The future tab refits the same model families to the entire selected range and forecasts beyond its end. Training RMSE/MAE/MAPE use in-sample residuals; validation measures compare holdout forecasts with held-out observations. The reported forms come from the training fit and can change after refitting to all data. Prediction intervals are model-based, not guarantees.

## Reproducibility and acknowledgment

The code was prepared with assistance from OpenAI's Codex. No nontrivial external code snippets were adapted. The test used R 4.5 and shiny 1.15.0, tsibble 1.2.0, fable 0.5.0, fabletools 0.8.0, feasts 0.5.0, distributional 0.9.0, dplyr 1.2.1, tidyr 1.3.2, and ggplot2 4.0.3. Packages are not locked; record `sessionInfo()` when deploying. Verify the deployed app and all controls with the course dataset before submission.

## Verification status

The app was exercised with Shiny's test server using Sparkling and Red (full range, 12-month holdout and horizon) and Rose and Dry white (1982-1993, 6-month holdout and horizon). All selected models returned finite accuracy metrics in these scenarios, and the history, forecast, validation, and specification outputs rendered. This is not exhaustive testing. The deployed app is at https://01a20ac4-d4e8-33f9-689a-9c683741a496.share.connect.posit.cloud/ .
