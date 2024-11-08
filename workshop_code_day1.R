# Hi! Welcome to forecasting :)
# Everything (almost) you need is in the fpp3 package
library(fpp3)

# tsibbles work like tibbles, but they have extra checks for time series.
# They can be filtered, mutated, summarised.
# If you want to change/aggregate the index, use index_by() instead of group_by()
PBS |> 
  filter(ATC2 == "A10") |> 
  index_by(Year = year(Month)) |> 
  summarise(Cost = sum(Cost))

# If you don't want a time series from your result, convert it back to a tibble.
PBS |> 
  filter(ATC2 == "A10") |> 
  as_tibble() |> 
  summarise(Cost = sum(Cost))

# Key variables can be grouped with group_by()
PBS |> 
  filter(ATC2 == "A10") |> 
  group_by(Concession) |> 
  index_by(Year = year(Month)) |> 
  summarise(Cost = sum(Cost)) |> 
  autoplot(Cost)

# Neat trick to calculate proportions within groups.
PBS |> 
  filter(ATC2 == "A10") |> 
  group_by_key() |> 
  index_by(Year = year(Month)) |> 
  mutate(Cost_Prop = Cost/sum(Cost)) |> 
  autoplot(Cost_Prop)

# Ungrouping will apply to both key and index groups.
PBS |> 
  filter(ATC2 == "A10") |> 
  group_by_key() |> 
  index_by(Year = year(Month)) |>
  ungroup()

# Let's make a tsibble
library(readxl)
download.file(
  "http://robjhyndman.com/data/tourism.xlsx",
  dl_tourism <- tempfile()
)
tourism <- read_excel(dl_tourism)

# Usually time columns are stored as dates - convert this into the appropriate
# granularity (week, month, quarter, etc.)
# 
# Then make a tsibble with as_tsibble, specifying the identifying columns (key)
# and the time column (index)
tourism <- tourism |> 
  mutate(Quarter = yearquarter(Quarter)) |> 
  as_tsibble(index = Quarter, 
             key = c(Region, State, Purpose))

tourism |> 
  as_tibble() |> 
  group_by(Region, Purpose) |> 
  summarise(avg_trips = mean(Trips)) |> 
  arrange(desc(avg_trips))

tourism |> 
  as_tibble() |> 
  group_by(Region, Purpose) |> 
  summarise(avg_trips = mean(Trips)) |> 
  ungroup() |> 
  filter(avg_trips == max(avg_trips))

tourism |> 
  as_tibble() |> 
  group_by(Region, Purpose) |> 
  summarise(avg_trips = mean(Trips)) |> 
  ungroup() |> 
  slice_max(avg_trips, n = 5)

tourism |> 
  group_by(State) |> 
  summarise(Trips = sum(Trips)) |> 
  autoplot(Trips)

# More examples - what patterns do you see?
aus_production |> 
  autoplot(Bricks)

aus_production |> 
  filter(!is.na(Bricks)) |> 
  autoplot(Bricks)

?pelt

pelt |> 
  autoplot(Lynx)

# Often each series has different scales
# Using facets with scales="free_y" will help see the patterns of each series
gafa_stock |> 
  autoplot(Close) +
  facet_grid(rows = vars(Symbol), scales = "free_y")

# Sometimes you have far too much data to plot
vic_elec |> 
  autoplot(Demand)

# You can aggregate it to see longer term patterns
vic_elec |> 
  index_by(Date) |> 
  summarise(Demand = sum(Demand)) |> 
  autoplot(Demand)

vic_elec |> 
  index_by(Week = yearweek(Date)) |> 
  summarise(Demand = sum(Demand)) |> 
  autoplot(Demand)

# Or zoom in to see the little details
vic_elec |> 
  filter(yearmonth(Time) == yearmonth("2014 Jan")) |> 
  autoplot(Demand)

# gg_season() also works for multiple seasonal patterns, change the period.
vic_elec |> 
  gg_season(Demand, period = "1 week")

# DIY version of gg_season() - harder than I thought, maybe {hms} package makes this easier.
vic_elec |> 
  transmute(
    make_datetime(
      hour = hour(Time),
      min = minute(Time)
    )
  )

vic_elec |> 
  ggplot(
    aes(
      x = make_datetime(
          hour = hour(Time),
          min = minute(Time)
      ),
      y = Demand,
      group = as.Date(Time)
    )
  ) + 
  geom_line() + 
  scale_x_datetime(date_labels = "%H:%M")

# Other geometries are often nice.
# Think about how you can make better plots than the default autoplot() or gg_*() helpers.
vic_elec |> 
  ggplot(
    aes(
      x = factor(make_datetime(
        hour = hour(Time),
        min = minute(Time)
      )),
      y = Demand
    )
  ) + 
  geom_boxplot()


# Another example
snowy <- tourism |> 
  filter(Region == "Snowy Mountains")

snowy |> 
  autoplot(Trips) + 
  facet_grid(
    rows = vars(Purpose),
    scales = "free_y"
  ) + 
  guides(colour = "none") +
  labs(title = "Snowy Mountain Tourism")

snowy |> 
  gg_season(Trips)

snowy |> 
  gg_subseries(Trips)

# Another way to show long and frequent (sub-daily frequency) time series is 
# with calendars. The {suggrants} package helps you do this with ggplot2.
library(sugrrants)

# facet_calendar()

pedestrian |> distinct(Sensor)

pedestrian |>
  filter(Sensor == "Southern Cross Station") |>
  mutate(Hour = hour(Date_Time)) |>
  frame_calendar(x = Hour, y = Count, date = Date, nrow = 4) |>
  ggplot(aes(x = .Hour, y = .Count, group = Date)) +
  geom_line() -> p1
prettify(p1,
         size = 3,
         label.padding = unit(0.15, "lines")
)

pedestrian |>
  filter(Sensor == "Southern Cross Station") |>
  autoplot(Count)

# Cycles vs Seasonality
# Plot side-by-side with vars()
pelt |> 
  autoplot(vars(Hare, Lynx))

aus_production |> 
  filter(!is.na(Bricks)) |> 
  autoplot(Bricks)

# Lag plots aren't as useful as ACF() plots since they are complicated
# But they are very similar to ACF, and are useful for learning the meaning of
# the ACF.
aus_production |> 
  filter(!is.na(Bricks)) |> 
  gg_lag(Bricks)

# ACF shows trends, random walks, seasonality, cycles, and little details (autocorrelation).
# The blue dashed line is the significance threshold, if they're within this interval
# there is no relationship / information.
aus_production |> 
  filter(!is.na(Bricks)) |> 
  ACF(Bricks) |> 
  autoplot()

pelt |> 
  autoplot(Lynx)

pelt |> 
  gg_lag(Lynx, lags = 1:16)

pelt |> 
  ACF(Lynx) |> 
  autoplot()

# Transforming your data can simplify your patterns for modelling and analysis
gafa_stock |> 
  filter(Symbol == "GOOG", year(Date) >= 2018) |> 
  autoplot(difference(Close))

gafa_stock |> 
  filter(Symbol == "GOOG", year(Date) >= 2018) |> 
  ACF(difference(Close)) |> 
  autoplot()

gafa_stock |> 
  filter(year(Date) >= 2018) |> 
  features(difference(Close), ljung_box, lag = 10)

# Per-capita adjustments
global_economy |> 
  transmute(gdp_pc = GDP / Population) |> 
  # mutate(gdp_pc) |> select(gdp_pc)
  autoplot(gdp_pc) + 
  guides(colour = "none")

# Highlight countries of interest
# Plot all countries with a grey line
# Show featured countries in colour
#
# Maybe a log y axis would be good here.
global_economy |> 
  ggplot(aes(x = Year, y = GDP / Population)) + 
  geom_line(aes(group = Country), colour = "grey80") +
  geom_line(
    aes(colour = Country), 
    data = global_economy |> filter(Country %in% c("Australia", "United States", "Italy", "China"))
  ) +
  theme_bw()

# Find and highlight the richest countries
# Don't forget to ignore NA!
rich_countries <- global_economy |> 
  as_tibble() |> 
  group_by(Country) |> 
  summarise(avg_gdp_pc = mean(GDP/Population, na.rm = TRUE)) |> 
  slice_max(avg_gdp_pc, n = 5)

# Joins are good! Worth learning :)
rich_5 <- global_economy |> 
  semi_join(rich_countries, by = "Country")

global_economy |> 
  ggplot(aes(x = Year, y = GDP / Population)) + 
  geom_line(aes(group = Country), colour = "grey80") +
  geom_line(
    aes(colour = Country), 
    data = rich_5
  ) +
  theme_bw()

# Mathematical transformations are also useful
# They help regularise the variance.
aus_production |> 
  autoplot(Gas)

aus_production |> 
  autoplot(sqrt(Gas))
# box_cox() is a generalisation of 'power transformations' - sqrt, log, ^-1, etc.
#
# Roughly (exact in shape)...
# lambda = 0 is log()
# lambda = 0.5 is sqrt()
# lambda = 1 is identity()
aus_production |> 
  autoplot(box_cox(Gas, 0.110))
aus_production |> 
  features(Gas, guerrero)

global_economy |> 
  filter(Code == "USA") |> 
  autoplot(GDP)
global_economy |> 
  filter(Code == "USA") |> 
  autoplot(box_cox(GDP, 0.282))

global_economy |> 
  filter(Code == "USA") |> 
  features(GDP, guerrero)

# STL decomposition splits a time series into patterns.
# y = trendcycle + season + remainder
# 
# You should transform the data into an additive pattern (constant variance) to use it.
tourism |> 
  group_by(State) |> 
  summarise(Trips = sum(Trips)) |> 
  model(STL(Trips))

# Sometimes mathematical transformations can't work.
# The variance needs to be proportional to the 'level of the series' (the y axis)
canadian_gas |> 
  autoplot(Volume)
canadian_gas |> 
  autoplot(log(Volume))
canadian_gas |> 
  autoplot(box_cox(Volume, 1))

# STL can still work with changing variance, just need to have 
# smaller (more flexible) windows.
dcmp <- canadian_gas |> 
  model(STL(Volume ~ trend(window = 33) + season(window = 7))) |> 
  components()
dcmp |> 
  autoplot()

# The components can also be plotted - it's nice to see the seasonality
# without the trend and noise. To rely on this, you need to check that the 
# decomposition is actually good.
dcmp |> 
  gg_subseries(season_year)

dcmp |> 
  autoplot(season_adjust)
dcmp |> 
  as_tsibble() |> 
  autoplot(season_adjust)

# Policy makers love seasonally adjusted data.
canadian_gas |> 
  autoplot(Volume, colour = "grey80") + 
  autolayer(dcmp, season_adjust) + 
  theme_bw()

library(GGally)
?ggpairs

flea
ggpairs(flea, columns = 2:4)

# Features are numerical summaries of a time series.
# STL features are great and intuitive - how strong is the trend? what about
# the seasonality? When is the seasonal peak/trough?
feat <- tourism |>
  features(Trips, feat_stl)
feat

# Features are very useful for visualising LOTS of time series at once.
feat |> 
  mutate(
    seasonal_peak_year = factor(seasonal_peak_year),
    seasonal_trough_year = factor(seasonal_trough_year)
  ) |> 
  ggpairs(columns = c(2, 4:7))

feat

# This code didn't work well - maybe you can fix it?
# The goal was to show each state and see what time of year most people go there.
# 
# A couple reasons why this was harder than it should be:
# * numbers 0-3 is bad for seasonal_peak_year, in the future it will say Q1, Q2, etc.
# * after a long day, I think all of our brains were becoming soup :)
feat |> 
  ggplot(
    aes(
      x = State, 
      fill = factor(seasonal_peak_year)
    )
  ) + 
  geom_bar(position = "dodge2")

feat |> 
  ggplot(
    aes(
      x = State, 
      fill = factor(seasonal_peak_year)
    )
  ) + 
  geom_boxplot()

# You can compute multiple features, here are some acf features too
PBS_feat <- PBS |>
  features(log(Cost), list(feat_stl, feat_acf))

PBS_feat

# Scatterplots of seasonal/trend strength are great plots of lots of series!
PBS_feat |> 
  ggplot(
    aes(
      x = trend_strength, 
      y = seasonal_strength_year
    )
  ) + 
  geom_point()

PBS_feat |> filter(is.na(seasonal_strength_year))

library(broom)
PBS_feat_narm <- PBS_feat |> 
  na.omit()

# Dimension reduction helps look for similar and outlying time series.
PBS_prcomp <- PBS_feat_narm |> 
  select(where(is.numeric)) |> 
  prcomp(scale = TRUE) |> 
  augment(PBS_feat_narm)
PBS_prcomp |> 
  ggplot(aes(x =.fittedPC1, y = .fittedPC2)) + 
  geom_point()

# Which time series were unlike the others?
outliers <- PBS_prcomp |> 
  filter(.fittedPC2 > 5)

semi_join(
  PBS,
  outliers
) |> 
  autoplot(Cost)

semi_join(
  PBS,
  outliers
) |> 
  model(STL(log(Cost))) |> 
  components() |> 
  autoplot()


# Some post workshop question answering code.
# STL needs additive time series, and the features were probably inaccurate because of this.
# Here we use automatically box_cox transformed time series - with a different lambda for each series.
# This should give better results than before.
PBS_lambda <- PBS |>
  features(Cost, guerrero)
PBS_lambda

PBS |> 
  left_join(PBS_lambda) |> 
  filter(is.na(lambda_guerrero)) |> 
  features(box_cox(Cost, lambda_guerrero), feat_stl)

# You can write custom features if you like - get inventive and submit cool features to {feasts}! :)
PBS |> 
  features(Cost, list(function(x) mean(x) + 1))
