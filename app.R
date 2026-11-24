# Australian wine forecasting explorer
# Run from this directory with shiny::runApp(). Data path is project-relative.
library(shiny)
library(tsibble)
library(fable)
library(fabletools)
library(feasts) # Required by automatic ARIMA seasonality selection.
library(dplyr)
library(tidyr)
library(ggplot2)
library(distributional)

raw <- read.csv('AustralianWines.csv', check.names = FALSE, stringsAsFactors = FALSE,
                na.strings = c('', 'NA', '*'))
names(raw) <- trimws(names(raw))
names(raw)[tolower(names(raw)) == 'sparkling'] <- 'Sparkling'
# The copy used here contains two internal missing observations for Rose.
# Interpolate them on the monthly index; preserve this fact for the UI.
wine <- raw |>
  mutate(Month = yearmonth(as.Date(paste0('01-', Month), format = '%d-%b-%y'))) |>
  pivot_longer(-Month, names_to = 'Varietal', values_to = 'Sales') |>
  group_by(Varietal) |>
  arrange(Month, .by_group = TRUE) |>
  mutate(Imputed = is.na(Sales),
         Sales = if (any(is.na(Sales))) approx(seq_along(Sales)[!is.na(Sales)],
                   Sales[!is.na(Sales)], xout = seq_along(Sales))$y else Sales) |>
  ungroup() |>
  as_tsibble(index = Month, key = Varietal)

ui <- fluidPage(
  titlePanel('Australian wine sales: compare forecasts'),
  p('Compare historical monthly sales and three forecasting methods. Sales are in thousands of litres.'),
  sidebarLayout(
    sidebarPanel(
      selectizeInput('varietals', 'Wine varieties', choices = sort(unique(wine$Varietal)),
                     selected = c('Sparkling', 'Red'), multiple = TRUE),
      dateRangeInput('dates', 'Historical date range', start = as.Date(min(wine$Month)),
                     end = as.Date(max(wine$Month)), min = as.Date(min(wine$Month)),
                     max = as.Date(max(wine$Month)), format = 'yyyy-mm-dd'),
      sliderInput('holdout', 'Validation months at end of range', 3, 24, 12, step = 1),
      sliderInput('horizon', 'Future forecast horizon (months)', 1, 24, 12, step = 1),
      checkboxGroupInput('models', 'Show models', c('TSLM', 'ETS', 'ARIMA'),
                         selected = c('TSLM', 'ETS', 'ARIMA')),
      helpText('Choose at least 36 training months plus the validation window. Models are fitted separately to each variety.'),
      downloadButton('download_metrics', 'Download accuracy table')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('History', plotOutput('history', height = '500px'),
                 p('Rose sales in July and August 1994 are missing in the source and are linearly interpolated for modeling and display.')),
        tabPanel('Forecasts', p('Models are refitted using all observations in the selected date range. Shaded bands are 95% prediction intervals.'),
                 plotOutput('future', height = '550px')),
        tabPanel('Validation', p('These forecasts use only the training period. Dashed black lines mark the start of the validation period; shaded bands are 95% prediction intervals.'),
                 plotOutput('backtest', height = '550px')),
        tabPanel('Accuracy', p('Training scores are in-sample residual accuracy. Validation scores are out-of-sample forecasts from the training fit. Lower is better; MAPE is a percentage.'),
                 tableOutput('metrics')),
        tabPanel('Model specifications', p('Specifications below describe the training fit used for validation. ETS and ARIMA are automatically selected; TSLM uses a linear trend and monthly seasonality.'),
                 tableOutput('specs')),
        tabPanel('About', p('Data: course-provided AustralianWines.csv; 180 monthly observations, January 1980–December 1994; six wine types.'),
                 p(a('Download the application source and data', href = 'wine-app-source.zip')),
                 p('Models use fable defaults for ETS and ARIMA. TSLM uses trend plus month-of-year seasonal effects. Two missing Rose values are linearly interpolated. This historical dataset should not be treated as a current market forecast.'),
                 p('The interval is a model-based 95% prediction interval; its reliability depends on the model assumptions. Model comparison uses the final months of the selected range as a holdout.'))
      )
    )
  )
)

server <- function(input, output, session) {
  selected <- reactive({
    req(length(input$varietals) > 0, length(input$dates) == 2)
    x <- wine |> filter(Varietal %in% input$varietals,
                        as.Date(Month) >= input$dates[1], as.Date(Month) <= input$dates[2])
    validate(need(nrow(x) > 0, 'Choose a date range with data.'))
    x
  })
  split_data <- reactive({
    x <- selected()
    months <- sort(unique(x$Month))
    validate(need(length(months) >= input$holdout + 36,
                  'Select a longer date range or reduce validation months: at least 36 training months are required.'))
    cutoff <- months[length(months) - input$holdout]
    list(train = x |> filter(Month <= cutoff), validation = x |> filter(Month > cutoff), cutoff = cutoff)
  })
  fit_models <- function(x) {
    x |> model(TSLM = TSLM(Sales ~ trend() + season()),
               ETS = ETS(Sales), ARIMA = ARIMA(Sales))
  }
  train_fit <- reactive({ fit_models(split_data()$train) })
  full_fit <- reactive({ fit_models(selected()) })
  validation_fc <- reactive({ forecast(train_fit(), h = input$holdout) })
  future_fc <- reactive({ forecast(full_fit(), h = input$horizon) })
  metrics <- reactive({
    tr <- accuracy(train_fit()) |> mutate(Window = 'Training')
    va <- accuracy(validation_fc(), split_data()$validation) |> mutate(Window = 'Validation')
    bind_rows(tr, va) |>
      filter(.model %in% input$models) |>
      select(Varietal, Model = .model, Window, RMSE, MAE, MAPE) |>
      arrange(Varietal, Model, Window) |>
      mutate(across(c(RMSE, MAE, MAPE), ~ round(.x, 2)))
  })
  output$history <- renderPlot({
    ggplot(selected(), aes(x = as.Date(Month), y = Sales)) +
      geom_line(color = '#21618c', linewidth = .65) +
      facet_wrap(~ Varietal, scales = 'free_y', ncol = 1) +
      labs(x = NULL, y = 'Sales (thousand litres)', title = 'Monthly sales') +
      theme_minimal(base_size = 13)
  })
  plot_fc <- function(fc, actual, title, cutoff = NULL) {
    req(length(input$models) > 0)
    f <- fc |> filter(.model %in% input$models) |>
      mutate(interval = hilo(Sales, 95), lower = interval$lower, upper = interval$upper,
             Date = as.Date(Month))
    p <- ggplot() +
      geom_ribbon(data = f, aes(x = Date, ymin = lower, ymax = upper, fill = .model), alpha = .12, show.legend = FALSE) +
      geom_line(data = actual, aes(x = as.Date(Month), y = Sales), color = 'grey30', linewidth = .5) +
      geom_line(data = f, aes(x = Date, y = .mean, color = .model), linewidth = .85) +
      facet_wrap(~ Varietal, scales = 'free_y', ncol = 1) +
      labs(title = title, x = NULL, y = 'Sales (thousand litres)', color = 'Model') +
      theme_minimal(base_size = 13) + theme(legend.position = 'bottom')
    if (!is.null(cutoff)) p <- p + geom_vline(xintercept = as.numeric(as.Date(cutoff)), linetype = 'dashed')
    p
  }
  output$future <- renderPlot({
    x <- selected(); max_month <- max(x$Month)
    history <- x |> filter(Month >= max_month - 35)
    plot_fc(future_fc(), history, 'Forecasts after the selected date range')
  })
  output$backtest <- renderPlot({
    s <- split_data()
    x <- selected() |> filter(Month >= s$cutoff - 23)
    plot_fc(validation_fc(), x, 'Holdout forecasts and actual sales', s$cutoff + 1)
  })
  output$metrics <- renderTable({ req(length(input$models) > 0); metrics() }, striped = TRUE, spacing = 's')
  output$specs <- renderTable({
    req(length(input$models) > 0)
    fit <- train_fit()
    rows <- lapply(seq_len(nrow(fit)), function(i) lapply(intersect(input$models, c('TSLM','ETS','ARIMA')), function(m) {
      spec <- tryCatch(model_sum(fit[[m]][[i]]), error = function(e) 'Model unavailable')
      data.frame(Varietal = as.character(fit$Varietal[i]), Model = m, Specification = spec)
    }))
    do.call(rbind, unlist(rows, recursive = FALSE))
  }, striped = TRUE, spacing = 's')
  output$download_metrics <- downloadHandler(filename = function() 'wine-model-accuracy.csv',
                                             content = function(file) write.csv(metrics(), file, row.names = FALSE))
}
shinyApp(ui, server)
