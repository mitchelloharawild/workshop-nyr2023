library(fpp3)

# Similar to yesterday, use the model() function to train a model specification
# to the dataset of choice.
#
# Here are the four benchmark methods.
brick_fit <- aus_production |>
  filter(!is.na(Bricks)) |>
  model(
    `Seasonal_naïve` = SNAIVE(Bricks),
    `Naïve` = NAIVE(Bricks),
    Drift = RW(Bricks ~ drift()),
    Mean = MEAN(Bricks)
  )

# The result is a mable (model table)
# There is 1 row per series, and the columns contain keys and models
brick_fit

# {broom} tools work here too
tidy(brick_fit)
augment(brick_fit)
glance(brick_fit)

# {tidyverse} functions also work
brick_fit |> 
  select(Mean)

# Produce forecasts from all models with forecast()
# `h = 5 years` specifies the forecast horizon (how far ahead we forecast)
# You can plot these forecasts with autoplot(), also pass in the data for history
# autoplot(level = 50) controls the confidence intervals shown
brick_fit |> 
  forecast(h = "5 years") |> 
  autoplot(
    data = aus_production |> filter(year(Quarter) >= 2000),
    level = c(50),#NULL#c(50, 60, 70, 80, 95)
    alpha = 0.5
  )

# The result is a fable (forecast table)
# A fable is like a tsibble, but has a distributional column of forecasts
brick_fc <- brick_fit |> 
  forecast(h = "5 years")
brick_fc

# The {distributional} package is used for working with the distribution
brick_fc |> 
  mutate(
    int90 = hilo(Bricks, 90),
    int90_upper = int90$upper,
    int90_lower = int90$lower,
    int90_level = int90$level,
    # mean(Bricks),
    # median(Bricks),
    # quantile(Bricks, 0.95)
  )

# Another example
hh_budget |> 
  autoplot(Wealth)

# This is an annual dataset, so the SNAIVE() model doesn't make sense here (no seasonality)
hh_budget |> 
  model(
    naive = NAIVE(Wealth),
    mean = MEAN(Wealth),
    drift = RW(Wealth ~ drift())
  ) |> 
  forecast(h = "5 years") |> 
  autoplot(hh_budget)

# distinct() is useful for finding the available key variables
aus_retail |> 
  distinct(Industry)
aus_retail |> 
  distinct(State)

# Calculate total australian takeaway turnover using dplyr
# Recall index is implicitly grouped
aus_takeout <- aus_retail |> 
  filter(Industry == "Takeaway food services") |> 
  summarise(Turnover = sum(Turnover))

aus_takeout |> 
  autoplot(Turnover)

aus_takeaway |> 
  model(
    mean = MEAN(Turnover),
    naive = NAIVE(Turnover),
    snaive = SNAIVE(Turnover),
    drift = RW(Turnover ~ drift())
  ) |> 
  select(-mean) |> 
  forecast(h = "10 years") |> 
  autoplot(aus_takeaway)

# You can create your own features
# Here we test the normality of the residuals
brick_fit |> 
  residuals() |> 
  features(.resid, ~ as_tibble(unclass(shapiro.test(.))))

aus_production |> 
  autoplot(Beer)

aus_production |> 
  model(SNAIVE(Beer)) |> 
  augment()
aus_production |> 
  model(SNAIVE(Beer)) |> 
  gg_tsresiduals()

# Is our model capturing all of the information?
# The ljung_box test checks the first few (8 here) lags of the ACF to see if they are jointly significant or not.
# If the ACF has no pattern, we call this white noise (we want this for our residuals)
aus_production |> 
  model(SNAIVE(Beer)) |> 
  augment() |> 
  features(.innov, 
           list(
             wn = ~ ljung_box(., lag = 8),
             normal = ~ as_tibble(unclass(shapiro.test(.)))
           )
  )
# Normal! :)
# but not white noise :(

# Try fitting a model on the first part of the data (up to 2007)
# Then we have future data withheld to see if our forecasts are accurate
beer_fit <- aus_production |>
  filter(between(year(Quarter), 1992, 2007)) |>
  model(
    snaive = SNAIVE(Beer),
    mean = MEAN(Beer)
  )

# In-sample accuracy on the model's training data
beer_fit |> 
  accuracy()

# Out-of-sample accuracy for the model's forecasts
beer_fit |> 
  forecast(h = "3 years") |> 
  accuracy(aus_production)

# By default you get point forecast accuracy, but there are measures for the
# accuracy of intervals and entire distributions.
point_accuracy_measures
interval_accuracy_measures
distribution_accuracy_measures

# Let's compare distribution accuracy
beer_fit |> 
  forecast(h = "3 years") |> 
  accuracy(aus_production, measures = distribution_accuracy_measures)

# Scale independent via relative performance
# 0 is same as benchmark, >0 is better.
skill_score()

# CRPS is scale dependent, but skill_score() gives a relative performance
# to a benchmark - making it scale independent.
beer_fit |> 
  forecast(h = "3 years") |> 
  accuracy(aus_production, measures = lst(skill_score(CRPS)))

# Another example
hh_budget |> 
  model(
    mean = MEAN(Wealth),
    naive = NAIVE(Wealth),
    drift = RW(Wealth ~ drift())
  ) |> 
  accuracy()


hh_budget |> as_tibble() |> summarise(max(Year))

hh_budget |> 
  # slice_head(n = -10)
  filter(Year < 2013) |> 
  model(
    mean = MEAN(Wealth),
    naive = NAIVE(Wealth),
    drift = RW(Wealth ~ drift())
  ) |> 
  forecast(h = "4 years") |> 
  accuracy(hh_budget) |> 
  arrange(Country)

# Here we add ETS() and play with some of it's options to get different forecasts
hh_fc <- hh_budget |> 
  # slice_head(n = -10)
  filter(Year < 2013) |> 
  model(
    mean = MEAN(Wealth),
    naive = NAIVE(Wealth),
    drift = RW(Wealth ~ drift()),
    tslm = TSLM(Wealth ~ trend()),
    aan = ETS(Wealth ~ error("A") + trend("A", beta = 0.4) + season("N"))
  ) |> 
  forecast(h = "4 years")

hh_fc |> 
  filter(.model == "aan") |> 
  autoplot(hh_budget)

# Is ETS better than the benchmarks for this data?
hh_fc |> 
  accuracy(hh_budget) |> 
  arrange(Country)

ets_fit <- hh_budget |> 
  model(
    aan = ETS(Wealth ~ error("A") + trend("A", beta = 0.4) + season("N"))
  )

ets_fit
tidy(ets_fit)

# ETS also has components(), but be mindful that it isn't used as a decomposition
# since it is based on lags of the patterns. It is however nice to check the 
# model's smoothing parameters (does the slope look good?)
ets_fit |> 
  filter(Country == "Australia") |> 
  components() |> 
  autoplot()

# Try damped trend ETS models
fit <- global_economy |> 
  filter(Country == "Australia") |> 
  model(holt = ETS(Population ~ trend("Ad", phi = 0.8)))

tidy(fit)
fit |>
  forecast(h = 50) |>
  autoplot(global_economy)


global_economy |> 
  filter(Country == "China") |>
  autoplot(GDP)
global_economy |> 
  filter(Country == "China") |>
  autoplot(log(GDP))


fit <- global_economy |> 
  filter(Country == "China") |>
  model(
    auto = ETS(GDP),
    aan = ETS(GDP ~ error("A") + trend("A") + season("N")),
    mmn = ETS(GDP ~ error("M") + trend("Md") + season("N")),
    ln_aan = ETS(log(GDP) ~ error("A") + trend("Ad") + season("N"))
  )

# Multiplicative trend models need simulation for forecasts, which takes longer to compute
# Set times to choose how many future paths are used.
fit |> 
  forecast(h = 20, times = 100) |> 
  autoplot(global_economy, level = 50, alpha = 0.5)

# Another example, let's try ETS on multiple series
aus_holidays <- tourism |> 
  filter(Purpose == "Holiday") |> 
  group_by(State) |> 
  summarise(Trips = sum(Trips)) 
aus_holidays |> 
  autoplot(Trips)
aus_holidays |> 
  model(ETS(Trips)) |> 
  tidy()
aus_holidays |> 
  model(ETS(Trips)) |> 
  components() |> 
  autoplot()

aus_holidays |> 
  model(ETS(Trips ~ error("A") + trend("A") + season("M"))) |> 
  forecast(h = "5 year") |> 
  autoplot(aus_holidays)

# Can ETS work for the weird seasonality in canadian_gas?
canadian_gas |> 
  autoplot(Volume)

fit <- canadian_gas |> 
  model(
    ETS(Volume ~ season("A"))
  )

tidy(fit)

fit |> 
  forecast(h = "4 years") |> 
  autoplot(canadian_gas)

components(fit) |> 
  autoplot()

fit |> 
  gg_tsresiduals()


fit |> 
  forecast(h = "4 years", bootstrap = TRUE, times = 100) |> 
  autoplot()


residuals(fit) |> 
  gg_subseries(.resid)

# Different number of days will affect production, sales, etc.
# So you can simplify patterns by dividing by scaling days_in_month(index).
canadian_gas |> 
  model(
    ETS(resp(Volume) / lubridate::days_in_month(Month))
  )
# Feature Request - this should work in the future
# days_in_month doesn't need an inverse since Month isn't being forecasted here.


## ARIMA models need stationary data - get it by transforming and differencing
global_economy |>
  filter(Country == "Australia") |> 
  autoplot(difference(Population, differences = 2))

fit <- global_economy |>
  filter(Country == "Australia") |> 
  model(arima = ARIMA(Population ~ pdq(0, 2, 2:5)))
fit |> 
  forecast(h = "10 years") |> 
  autoplot(global_economy)

global_economy |> 
  filter(Country == "United States") |> 
  autoplot(box_cox(GDP, 0.3))

global_economy |> 
  filter(Country == "United States") |> 
  autoplot(difference(box_cox(GDP, 0.3)))
  

global_economy |> 
  filter(Country == "United States") |> 
  ACF(difference(box_cox(GDP, 0.3))) |> 
  autoplot()

global_economy |> 
  filter(Country == "United States") |> 
  gg_tsdisplay(difference(box_cox(GDP, 0.3)),
               plot_type = "partial")

global_economy |> 
  filter(Country == "United States") |> 
  model(
    ARIMA(box_cox(GDP, 0.3) ~ pdq(d=1))
  ) |> 
  forecast(h="10 years") |> 
  autoplot(global_economy)

?ARIMA
?unitroot_options

# Applying ARIMA to another example
snowy_holiday <- tourism |> 
  filter(Purpose == "Holiday",
         Region == "Snowy Mountains")

snowy_holiday |> 
  autoplot(Trips)

fit <- snowy_holiday |> 
  model(ARIMA(Trips)) 
fit |> 
  forecast(h = "3 years") |> 
  autoplot(snowy_holiday)
fit |> 
  gg_tsresiduals()

# Is ARIMA better than ETS?
fit <- snowy_holiday |> 
  model(
    arima = ARIMA(Trips),
    ets = ETS(Trips)
  ) 

fit
accuracy(fit)

fit |> 
  forecast(h = "3 years") |> 
  autoplot(snowy_holiday)

fit <- snowy_holiday |> 
  model(
    arima = ARIMA(Trips),
    ets = ETS(Trips)
  ) 

# Use an ensemble - an average between ets and arima. Is it better?
fit |> 
  mutate(comb = (arima + ets)/2) |> 
  accuracy()

# Dynamic regression is lm with ARIMA errors.
# Just add regressors to ARIMA() for this!
fit <- us_change |>
  slice_head(n=-8) |> 
  model(
    tslm = TSLM(Consumption ~ Income + Production + Savings + Unemployment),
    regarima = ARIMA(Consumption ~ Income + Production + Savings + Unemployment)
  )
fit

# To forecast with regressors, you need to know future values of these regressors
# Often this is just as hard to predict as your response variable.
# To specify what happens in the future, create a tsibble of future time points
# and values, and use forecast(new_data = ???).
#
# The new_data(<tsibble>, h) function can get you started.
fit |> 
  forecast(new_data = slice_tail(us_change, n = 8) |> select(-Consumption)) |>      #(h = "2 years")
  autoplot(us_change)
accuracy(fit)
gg_tsresiduals(fit)

# Reconciliation involves forecasting the aggregates of the series too.
# aggregate_key() computes all possible aggregates.
# * is for grouped terms, and / for nested or hierarchical terms.
# Works like summarise()
tourism |>
  aggregate_key(Purpose * (State / Region), Trips = sum(Trips)) |> 
  filter(Purpose == "Business")

# This can take a while to model, as there are lots of time series.
# Use with_progress() for a progress bar.
progressr::with_progress(
  fit <- tourism |>
    aggregate_key(Purpose * (State / Region), Trips = sum(Trips)) |>
    model(ets = ETS(Trips ~ error("A") + trend("A") + season("A")))
)

fit

# The reconcile() function makes the forecasts coherent (they add up)
# This usually improves accuracy, and gives more sensible forecasts.
fit |> 
  filter(is_aggregated(State)) |>
  select(Purpose, ets) |> 
  reconcile(ets_coherent = min_trace(ets)) |> 
  forecast(h = 1) |> 
  arrange(.model, Purpose)

fc <- fit |> 
  reconcile(ets_coherent = min_trace(ets)) |> 
  forecast(h = "2 years")

tourism_agg <- tourism |>
  aggregate_key(Purpose * (State / Region), Trips = sum(Trips))

tourism_snowy <- tourism_agg |> 
  filter(Region == "Snowy Mountains") |> 
  select(-Region, -State)

fc |> 
  filter(Region == "Snowy Mountains") |> 
  select(-Region, -State) |> 
  autoplot(tourism_snowy)

# Another example - lab exercise
agg_pbs <- PBS |> 
  aggregate_key(
    Concession * Type * ATC1,
    Scripts = sum(Scripts),
    Cost = sum(Cost)
  )

agg_pbs |> 
  filter(is_aggregated(Concession), is_aggregated(ATC1)) |> 
  autoplot(Cost)

progressr::with_progress(
  agg_fit <- agg_pbs |> 
    model(
      ets = ETS(Scripts ~ error("A") + trend("A") + season("A")),
      arima = ARIMA(log(Scripts + 1)),
      snaive = SNAIVE(Scripts)
    )
)

fc <- agg_fit |> 
  select(ets) |> 
  reconcile(ets_coherent = min_trace(ets)) |> 
  forecast()

fc |> 
  filter(
    ATC1 == "A",
    is_aggregated(Concession),
    is_aggregated(Type)
  ) |> 
  arrange(Month)
