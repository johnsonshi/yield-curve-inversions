#!/usr/bin/env Rscript

################################################################################
### Most analysts only report on the 10yr/2yr or 10yr/3mo yield curves. This
### can often lead to ambiguity. For instance, the 10yr/2yr curve barely
### inverted for 2 days in 2019, so there was a lot of talk about whether that
### inversion "counted" towards a recession.
###
### Instead of looking at individual yield curves, this script computers the
### number of yield curves inverted at any given time. It also returns the
### percent of all yield curves that are currently inverted, because the number
### of maturities has changed over the years, and this helps provide a more
### apples-to-apples comparison.
###
### Originally inspired by https://www.reddit.com/user/MetricT/
### Original post: https://www.reddit.com/r/dataisbeautiful/comments/pa18pd/oc_number_of_inverted_yield_curves_as_a_percent/
################################################################################

# TODO/IDEAS:
#   cpi
#   ppi all commodities
#   sticky price cpi
#   yields on constant maturity basis and secondary market rate
#   2 month treasury yield is missing

################################################################################
### Load script libraries
################################################################################

# Load necessary libraries
library(tidyverse)
library(fredr)
library(scales)
library(lubridate)
library(forecast)
library(cowplot)
library(TTR)

################################################################################
### Get script args and set script configs
################################################################################

print_required_args_and_stop <- function() {
  stop("args: (1-required) output file path, (2-optional) lookback period from current date (integer) in years")
}

args = commandArgs(trailingOnly = TRUE)

# Output file path is a required argument if not run in interactive mode.
if (!interactive()) {
  if (length(args) < 1) {
    print_required_args_and_stop()
  }

  output_file_path = args[1]
  if (!is.na(output_file_path)) {
    print(paste("script output", output_file_path, sep = ": "))
  }
}

################################################################################
### Graph Start and End Date
################################################################################

# Graph end date. Set to "as.Date(Sys.Date())" to set today as the end date.
date_now <- as.Date(Sys.Date())
graph_end <- date_now

# Lookback period is an optional argument.
if (length(args) >= 2) {
  lookback_years = strtoi(args[2])

  if (is.na(lookback_years)) {
    print_required_args_and_stop()
  }
  
  if (lookback_years <= 0) {
    stop("lookback period should be greater than 0")
  }
  
  if (lookback_years > 30) {
    stop("lookback period cannot exceed 30 years")
  }

  graph_start <- graph_end - as.difftime(lookback_years * 52, unit="weeks")
} else {
  # Graph start date. Earlier dates have fewer yield curves, so they will
  # appear shorter (top graph) or coarser (bottom graph).  For that reason, I
  # recommend applying a 2-week moving average to the bottom graph, though that
  # is strictly for aesthetics.
  graph_start <- as.Date("1960-01-01")
}

print(paste("graph start", format(graph_start, "%b-%d-%Y"), sep = ": "))
print(paste("graph end", format(graph_end, "%b-%d-%Y"), sep = ": "))

################################################################################
### Graph Smoothing
################################################################################

# For a 2-week SMA we actually use 15 days because we need to use an odd number
# Set to 1 to just graph the raw data
SMA_days = 1

# Set your FRED API key here. You may request an API key at:
# https://research.stlouisfed.org/useraccount/apikeys
api_key_fred <- ""
fredr_set_key(api_key_fred)

################################################################################
### Add recession bars to graphs
################################################################################
geom_recession_bars <- function(date_start, date_end, fill = "darkseagreen4") {
  
  date_start <- as.Date(date_start, origin = "1970-01-01")
  date_end   <- as.Date(date_end,   origin = "1970-01-01")
  
  recessions_tibble <-
    tribble(
      ~peak,     ~trough,
      "1960-04-01", "1961-02-01",
      "1969-12-01", "1970-11-01",
      "1973-11-01", "1975-03-01",
      "1980-01-01", "1980-07-01",
      "1981-07-01", "1982-11-01",
      "1990-07-01", "1991-03-01",
      "2001-03-01", "2001-11-01",
      "2007-12-01", "2009-06-01",
      "2020-02-01", "2020-04-01"
    ) %>%
    mutate(peak   = as.Date(peak),
           trough = as.Date(trough))
  
  recessions_trim <- recessions_tibble %>%
    filter(peak   >= min(date_start) &
             trough <= max(date_end))
  
  if (nrow(recessions_trim) > 0) {
    
    recession_bars <- geom_rect(data        = recessions_trim,
                                inherit.aes = F,
                                fill        = fill,
                                alpha       = 0.25,
                                aes(xmin = as.Date(peak,   origin = "1970-01-01"),
                                    xmax = as.Date(trough, origin = "1970-01-01"),
                                    ymin = -Inf, ymax = +Inf))
  } else {
    recession_bars <- geom_blank()
  }
}

################################################################################
### Pull stock market index data from FRED
################################################################################

stock_mkt_idx_fred_series_ids <- c("WILL5000PRFC")

### Use purrr to pull all of the series from FRED
stock_mkt_idx_values <-
  purrr::map_dfr(stock_mkt_idx_fred_series_ids,
                 .f = ~ fredr(series_id = .x, frequency = "d")) %>%
  select(date, series_id, value) %>%
  pivot_wider(id_cols = "date",
              names_from = "series_id",
              values_from = "value") %>%
  arrange(date)

# Data from older time frames (before the 1980s) is sparse and
# and is not fine-grained (lots of NA values interspersed throughout the dates).
# For dates with NA index values, set the index value to the last known value.
for (row_num in 2:nrow(stock_mkt_idx_values)) {
  if (is.na(stock_mkt_idx_values[row_num, "WILL5000PRFC"])) {
    stock_mkt_idx_values[row_num, "WILL5000PRFC"] <-
      stock_mkt_idx_values[row_num-1, "WILL5000PRFC"]
  }
}

# Apply log() to the stock market index values.
stock_mkt_idx_values$log_WILL5000PRFC <- log(stock_mkt_idx_values$WILL5000PRFC)

################################################################################
### Pull volatility data from FRED
################################################################################

cboe_volatility_vix_id <- c("VIXCLS")

### Use purrr to pull all of the series from FRED
vix_values <-
  purrr::map_dfr(cboe_volatility_vix_id,
                 .f = ~ fredr(series_id = .x, frequency = "d")) %>%
  select(date, series_id, value) %>%
  pivot_wider(id_cols = "date",
              names_from = "series_id",
              values_from = "value") %>%
  arrange(date)

# Data from older time frames (before the 1980s) is sparse and
# and is not fine-grained (lots of NA values interspersed throughout the dates).
# For dates with NA index values, set the index value to the last known value.
for (row_num in 2:nrow(vix_values)) {
  if (is.na(vix_values[row_num, "VIXCLS"])) {
    vix_values[row_num, "VIXCLS"] <-
      vix_values[row_num-1, "VIXCLS"]
  }
}

################################################################################
### Pull Treasury data from FRED
################################################################################

### Use purrr to pull all of the series from FRED

reg_treasuries <- c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS3",
                    "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")
rates <-
  purrr::map_dfr(reg_treasuries,
                 .f = ~ fredr(series_id = .x, frequency = "d")) %>%
  select(date, series_id, value) %>%
  pivot_wider(id_cols = "date",
              names_from = "series_id",
              values_from = "value") %>%
  arrange(date)

rate_spread_ids <- c("T10Y2Y", "T10Y3M")
rate_spreads <-
  purrr::map_dfr(rate_spread_ids,
                 .f = ~ fredr(series_id = .x, frequency = "d")) %>%
  select(date, series_id, value) %>%
  pivot_wider(id_cols = "date",
              names_from = "series_id",
              values_from = "value") %>%
  arrange(date)

interest_rate_ids <- c("FEDFUNDS", "DPRIME", "INTDSRUSM193N", "SOFR")
interest_rates <-
  purrr::map_dfr(interest_rate_ids,
                 .f = ~ fredr(series_id = .x, frequency = "m")) %>%
  select(date, series_id, value) %>%
  pivot_wider(id_cols = "date",
              names_from = "series_id",
              values_from = "value") %>%
  arrange(date)

inflation_unemployment_ids <- c("T10YIE", "T5YIFR", "T5YIE", "MICH", "UNRATE")
inflation_unemployment_rates <-
  purrr::map_dfr(inflation_unemployment_ids,
                 .f = ~ fredr(series_id = .x, frequency = "m")) %>%
  select(date, series_id, value) %>%
  pivot_wider(id_cols = "date",
              names_from = "series_id",
              values_from = "value") %>%
  arrange(date)

pricing_ids <- c("PCETRIM12M159SFRBDAL", "CORESTICKM159SFRBATL", "CORESTICKM158SFRBATL")
pricing_rates <-
  purrr::map_dfr(pricing_ids,
                 .f = ~ fredr(series_id = .x, frequency = "m")) %>%
  select(date, series_id, value) %>%
  pivot_wider(id_cols = "date",
              names_from = "series_id",
              values_from = "value") %>%
  arrange(date)

################################################################################
### Compute the number of inverted yield curves
################################################################################

all_yield_curves <-
  rates %>%
  mutate(
    yc_30_20 = DGS30 - DGS20,
    yc_30_10 = DGS30 - DGS10,
    yc_30_7  = DGS30 - DGS7,
    yc_30_5  = DGS30 - DGS5,
    yc_30_3  = DGS30 - DGS3,
    yc_30_2  = DGS30 - DGS2,
    yc_30_1  = DGS30 - DGS1,
    yc_30_6m = DGS30 - DGS6MO,
    yc_30_3m = DGS30 - DGS3MO,
    yc_30_1m = DGS30 - DGS1MO,
    yc_20_10 = DGS20 - DGS10,
    yc_20_7  = DGS20 - DGS7,
    yc_20_5  = DGS20 - DGS5,
    yc_20_3  = DGS20 - DGS3,
    yc_20_2  = DGS20 - DGS2,
    yc_20_1  = DGS20 - DGS1,
    yc_20_6m = DGS20 - DGS6MO,
    yc_20_3m = DGS20 - DGS3MO,
    yc_20_1m = DGS20 - DGS1MO,
    yc_10_7  = DGS10 - DGS7,
    yc_10_5  = DGS10 - DGS5,
    yc_10_3  = DGS10 - DGS3,
    yc_10_2  = DGS10 - DGS2,
    yc_10_1  = DGS10 - DGS1,
    yc_10_6m = DGS10 - DGS6MO,
    yc_10_3m = DGS10 - DGS3MO,
    yc_10_1m = DGS10 - DGS1MO,
    yc_7_5   = DGS7  - DGS5,
    yc_7_3   = DGS7  - DGS3,
    yc_7_2   = DGS7  - DGS2,
    yc_7_1   = DGS7  - DGS1,
    yc_7_6m  = DGS7  - DGS6MO,
    yc_7_3m  = DGS7  - DGS3MO,
    yc_7_1m  = DGS7  - DGS1MO,
    yc_5_3   = DGS5  - DGS3,
    yc_5_2   = DGS5  - DGS2,
    yc_5_1   = DGS5  - DGS1,
    yc_5_6m  = DGS5  - DGS6MO,
    yc_5_3m  = DGS5  - DGS3MO,
    yc_5_1m  = DGS5  - DGS1MO,
    yc_3_2   = DGS3  - DGS2,
    yc_3_1   = DGS3  - DGS1,
    yc_3_6m  = DGS3  - DGS6MO,
    yc_3_3m  = DGS3  - DGS3MO,
    yc_3_1m  = DGS3  - DGS1MO,
    yc_2_1   = DGS2  - DGS1,
    yc_2_6m  = DGS2  - DGS6MO,
    yc_2_3m  = DGS2  - DGS3MO,
    yc_2_1m  = DGS2  - DGS1MO,
    yc_1_6m  = DGS1  - DGS6MO,
    yc_1_3m  = DGS1  - DGS3MO,
    yc_1_1m  = DGS1  - DGS1MO,
    yc_6m_3m = DGS6MO - DGS3MO,
    yc_6m_1m = DGS6MO - DGS1MO,
    yc_3m_1m = DGS3MO - DGS1MO
  ) %>%
  select(date, starts_with("yc")) %>%
  pivot_longer(-date, names_to = "maturity", values_to = "value") %>%
  filter(!is.na(value))

# Count the number of yield curves on a given date
yield_curve_count <-
  all_yield_curves %>%
  group_by(date) %>%
  summarize(total_curves = n()) %>%
  ungroup()

# If you want to see the # of yield curves over time
#yield_curve_count %>% plot()

# Count the number of inverted yield curves at a given date, and
# compute the percent of yield curves inverted
all_inverts <-
  all_yield_curves %>%
  select(date) %>%
  mutate(blank = "") %>%
  unique() %>%
  arrange(date) %>%
  full_join(all_yield_curves %>%
              filter(value < 0) %>%
              group_by(date) %>%
              summarize(n_inverts = n()) %>%
              ungroup(), by = "date") %>%
  mutate(n_inverts = ifelse(is.na(n_inverts), 0, n_inverts)) %>%
  select(date, n_inverts) %>%
  left_join(yield_curve_count, by = "date") %>%
  mutate(n_per = n_inverts / total_curves)

################################################################################
### Get latest records
################################################################################

last_stock_mkt_idx_record <- tail(stock_mkt_idx_values, n = 1)
print(last_stock_mkt_idx_record)

last_rates <- tail(rates, n = 1)
print(last_rates)

last_num_inverts <- tail(all_inverts, n = 1)
print(last_num_inverts)

################################################################################
### Graphs
################################################################################

# Manually invoke the garbage collector before proceeding to free up memory.
gc()

g_stock_mkt_index_values_log <-
  stock_mkt_idx_values %>%
  filter(date >= as.Date(graph_start)) %>%
  ggplot() +
  theme_bw() +
  geom_line(aes(x = as.Date(date), y = log_WILL5000PRFC)) +
  geom_recession_bars(as.Date(graph_start), as.Date(graph_end)) +
  scale_x_date(breaks = date_breaks("1 year"), date_labels = "%Y", limits = c(graph_start, graph_end)) +
  labs(x = "", y = "",
    title =
      paste(
        "US Stock Market Indexes (log-adjusted).",
        "WILL5000PRFC Index Value as of", format(last_stock_mkt_idx_record$date, "%b-%d-%Y:"),
        last_stock_mkt_idx_record$WILL5000PRFC, "(raw index value).",
        sprintf(last_stock_mkt_idx_record$log_WILL5000PRFC, fmt = '%#.5f'), "(log-adjusted index value).",
        sep = " "))

g_vix_values <-
  vix_values %>%
  filter(date >= as.Date(graph_start)) %>%
  ggplot() +
  theme_bw() +
  geom_line(aes(x = as.Date(date), y = VIXCLS)) +
  geom_recession_bars(as.Date(graph_start), as.Date(graph_end)) +
  scale_x_date(breaks = date_breaks("1 year"), date_labels = "%Y", limits = c(graph_start, graph_end)) +
  labs(x = "", y = "",
       title =
         paste(
           "CBOE Volatility Index (VIX)",
           "VIX Value as of", format(vix_values$date, "%b-%d-%Y:"),
           vix_values$VIXCLS, "(raw index value).",
           sep = " "))

g_pricing_rates <-
  pricing_rates %>%
  filter(date >= as.Date(graph_start)) %>%
  ggplot() +
  theme_bw() +
  geom_line(aes(x = as.Date(date), y = PCETRIM12M159SFRBDAL), color = "red2") +
  geom_line(aes(x = as.Date(date), y = CORESTICKM159SFRBATL), color = "springgreen3") +
  geom_line(aes(x = as.Date(date), y = CORESTICKM158SFRBATL), color = "tan3") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_recession_bars(as.Date(graph_start), as.Date(graph_end)) +
  scale_x_date(breaks = date_breaks("1 year"), date_labels = "%Y", limits = c(graph_start, graph_end)) +
  labs(x = "", y = "", title = "US Price Indexes: Trimmed Mean PCE Inflation PCETRIM12M159SFRBDAL [red], Sticky CPI Less F&E - % Change 1Y Ago CORESTICKM159SFRBATL [green], Sticky CPI Less F&E - % Change Annualized CORESTICKM158SFRBATL [yellow]")

g_inflation_unemployment_rates <-
  inflation_unemployment_rates %>%
  filter(date >= as.Date(graph_start)) %>%
  ggplot() +
  theme_bw() +
  geom_line(aes(x = as.Date(date), y = T10YIE), color = "red2") +
  geom_line(aes(x = as.Date(date), y = T5YIFR), color = "springgreen3") +
  geom_line(aes(x = as.Date(date), y = T5YIE), color = "tan3") +
  geom_line(aes(x = as.Date(date), y = MICH), color = "navyblue") +
  geom_line(aes(x = as.Date(date), y = UNRATE), color = "cyan") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_recession_bars(as.Date(graph_start), as.Date(graph_end)) +
  scale_x_date(breaks = date_breaks("1 year"), date_labels = "%Y", limits = c(graph_start, graph_end)) +
  labs(x = "", y = "", title = "US Inflation Rates: 10Y Breakeven T10YIE [red], 5Y Forward Inflation Expectation T5YIFR [green], 5Y Breakeven T5YIE [yellow], UMich Inflation Expectation MICH [blue], Unemployment UNRATE [cyan]")

g_interest_rates <-
  interest_rates %>%
  filter(date >= as.Date(graph_start)) %>%
  ggplot() +
  theme_bw() +
  geom_line(aes(x = as.Date(date), y = FEDFUNDS), color = "red2") +
  geom_line(aes(x = as.Date(date), y = SOFR), color = "springgreen3") +
  geom_line(aes(x = as.Date(date), y = INTDSRUSM193N), color = "tan3") +
  geom_line(aes(x = as.Date(date), y = DPRIME), color = "navyblue") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_recession_bars(as.Date(graph_start), as.Date(graph_end)) +
  scale_x_date(breaks = date_breaks("1 year"), date_labels = "%Y", limits = c(graph_start, graph_end)) +
  labs(x = "", y = "", title = "US Interest Rates: FEDFUNDS [red], SOFR [green], Discount Rate INTDSRUSM193N [yellow], DPRIME [blue]")

g_treasury_rates <-
  rates %>%
  filter(date >= as.Date(graph_start)) %>%
  ggplot() +
  theme_bw() +
  geom_line(aes(x = as.Date(date), y = DGS1MO), color = "gray65") +
  geom_line(aes(x = as.Date(date), y = DGS3MO), color = "gray60") +
  geom_line(aes(x = as.Date(date), y = DGS6MO), color = "gray55") +
  geom_line(aes(x = as.Date(date), y = DGS1), color = "gray50") +
  geom_line(aes(x = as.Date(date), y = DGS2), color = "gray45") +
  geom_line(aes(x = as.Date(date), y = DGS2), color = "gray40") +
  geom_line(aes(x = as.Date(date), y = DGS2), color = "gray35") +
  geom_line(aes(x = as.Date(date), y = DGS7), color = "gray30") +
  geom_line(aes(x = as.Date(date), y = DGS10), color = "gray25") +
  geom_line(aes(x = as.Date(date), y = DGS20), color = "gray20") +
  geom_line(aes(x = as.Date(date), y = DGS30), color = "gray15") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_recession_bars(as.Date(graph_start), as.Date(graph_end)) +
  scale_x_date(breaks = date_breaks("1 year"), date_labels = "%Y", limits = c(graph_start, graph_end)) +
  labs(x = "", y = "", title = "US Treasury Yields (Lighter -> Shorter Duration, Darker -> Longer Duration)")
  

g_common_yield_spreads <-
  rate_spreads %>%
  filter(date >= as.Date(graph_start)) %>%
  ggplot() +
  theme_bw() +
  geom_line(aes(x = as.Date(date), y = T10Y2Y), color = "red2") +
  geom_line(aes(x = as.Date(date), y = T10Y3M), color = "navyblue") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_recession_bars(as.Date(graph_start), as.Date(graph_end)) +
  scale_x_date(breaks = date_breaks("1 year"), date_labels = "%Y", limits = c(graph_start, graph_end)) +
  labs(x = "", y = "", title = "US Yield Spreads: T10Y2Y [red], T10Y3M [blue]")

g_num_inverts <-
  all_inverts %>%
  filter(date >= as.Date(graph_start)) %>%
  ggplot() +
  theme_bw() +
  geom_line(aes(x = as.Date(date) - (SMA_days - 1)/2, y = SMA(n_inverts, n = SMA_days))) +
  geom_recession_bars(as.Date(graph_start), as.Date(graph_end)) +
  scale_x_date(breaks = date_breaks("1 year"), date_labels = "%Y", limits = c(graph_start, graph_end)) +
  labs(x = "", y = "",
    title =
      paste(
        "Number of Inverted US Yield Curves.",
        "Number of Inversions as of", format(last_num_inverts$date, "%b-%d-%Y:"),
        last_num_inverts$n_inverts,
        sep = " "))

g_per_inverts <-
  all_inverts %>%
  filter(date >= as.Date(graph_start)) %>%
  ggplot() +
  theme_bw() +
  # n in SMA() should be an odd number, then subtract (n-1/2) from as.Date(date)
  geom_line(aes(x = as.Date(date) - (SMA_days - 1)/2, y = SMA(n_per, n = SMA_days))) +
  geom_recession_bars(as.Date(graph_start), as.Date(graph_end)) +
  scale_x_date(breaks = date_breaks("1 year"), date_labels = "%Y", limits = c(graph_start, graph_end)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = pretty_breaks(5)) +
  labs(x = "", y = "",
      title =
        paste(
          "Number of Inverted US Yield Curves as a % of All US Yield Curves.",
          "% of Inversions as of", format(last_num_inverts$date, "%b-%d-%Y:"),
          label_percent()(last_num_inverts$n_per),
          sep = " "))

################################################################################
### Output
################################################################################

# 1. Open jpeg file
if (!is.na(output_file_path)) {
  jpeg(output_file_path, width = 2160, height = 1620)
}

# 2. Create the plot
print(
  plot_grid(
    g_inflation_unemployment_rates,
    g_pricing_rates,
    g_interest_rates,
    g_treasury_rates,
    g_common_yield_spreads,
    g_stock_mkt_index_values_log,
    g_vix_values,
    g_num_inverts,
    g_per_inverts,
    nrow = 9,
    ncol = 1,
    align = "hv"
  )
)

# 3. Close the file
if (!is.na(output_file_path)) {
  dev.off()
}

