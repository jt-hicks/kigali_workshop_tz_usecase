############################################################
# Modelling forecast uncertainty (Workshop Exercises)
# Harsha Halgamuwe Hewage | DL4SG, Cardiff University
# Date: 2026-02-25
#
# Goal:
#   1) Model-based uncertainty (ARIMAX distributions + prediction intervals)
#   2) Bootstrap uncertainty (sample paths + bootstrap intervals)
#   3) Conformal prediction (split/cv style calibration -> valid PI)
#
# How to use this script:
#   - Run section-by-section (Ctrl/Cmd + Enter).
#   - Do NOT rush to the end. Each "YOUR TURN" is an exercise.
############################################################


############################
# 0) Housekeeping
############################

# Clear workspace (optional)
rm(list = ls())

# If you want reproducibility for bootstrap simulations:
set.seed(123)

# ---- Packages: install if missing, then load ----
required_packages <- c(
  "tidyverse",
  "ggthemes",
  "tsibble",
  "feasts",
  "fable",
  "distributional",
  "conformalForecast",
  "forecast",
  "ggdist"        # used for half-eye distribution plot
)

new_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(new_packages)) {
  install.packages(new_packages, dependencies = TRUE)
}

invisible(lapply(required_packages, library, character.only = TRUE))


##TZ data try
tz_data <- read.csv('C:/Users/jthicks/OneDrive - Imperial College London/Imperial_ResearchAssociate/Meetings/Kigali_DHIS2_workshop_2026/council_monthly_export_2026-02-24.csv')
tz_data <- tz_data |>
  mutate(time_period = yearmonth(month)) |>
  mutate(disease_cases = log(opd_cases)) |>
  as_tsibble(index = time_period, key = council)

# Here we use:
#   Train: 2018 to 2022 Dec
#   Future: from 2023 Jan onwards (exogenous variables available, disease_cases removed)
tz_data_train <- tz_data |>
  filter_index("2018 Jan" ~ "2022 Dec")

tz_data_future <- tz_data |>
  filter_index("2023 Jan" ~ .) |>
  select(-disease_cases)

# Check
range(tz_data_train$time_period)
range(tz_data_future$time_period)


############################################################
# 3) MODEL-BASED UNCERTAINTY (ARIMAX via fable)
############################################################

# Fit ARIMAX for one location
fit_arimax <- tz_data_train |>
  # filter(council == "Lindi Municipal Council") |>
  model(arimax = ARIMA(disease_cases ~ rainfall))

report(fit_arimax)

# Forecast distribution for the future period
fc_arimax <- fit_arimax |>
  forecast(new_data = tz_data_future)

# ---- Visualise forecast distribution (half-eye) ----
# We also truncate distributions at 0 so we don't show negative cases.
fcst <- fc_arimax |> 
  mutate(disease_cases = distributional::dist_truncated(disease_cases, lower = 0), .mean=mean(disease_cases))

region_council_key <- as_tibble(tz_data) |> select(region_id, council) |> distinct()

fitted_arimax <- fit_arimax |> augment()

fitted_arimax <- fitted_arimax|>
  left_join(as_tibble(region_council_key), by = "council")

region_to_filter <- 'Ruvuma'

fitted_arimax_sub <- fitted_arimax |>
  filter(region_id == region_to_filter)
fcst_sub <- fcst|> filter(region_id == region_to_filter)

plot <- ggplot(data = fcst_sub, mapping = aes(x = time_period, ydist = exp(disease_cases)))+
  ggdist::stat_halfeye(alpha = .4) +
# ggplot(data = fcst|> filter(council == council_to_filter),aes(x = time_period))+
  geom_line(aes(y=exp(.mean), colour ="Point Forecast")) +
  geom_line(aes(y = exp(.fitted), colour ="Fitted"), data = filter_index(fitted_arimax_sub, "2020 Jan" ~ .)) +
  geom_point(aes(y = exp(.fitted), colour ="Fitted"), data = filter_index(fitted_arimax_sub, "2020 Jan" ~ .)) +
  geom_line(aes(y = exp(disease_cases), colour ="Actual"), data = filter_index(tz_data |> filter(region_id == region_to_filter), "2023 Jan" ~ .)) +
  geom_point(aes(y = exp(disease_cases), colour ="Actual"), data = filter_index(tz_data |> filter(region_id == region_to_filter), "2023 Jan" ~ .))+
  scale_color_manual(name=NULL,
                     breaks=c('Fitted', 'Actual',"Point Forecast"),
                     values=c('Fitted'='#E69F00', 'Actual'='#0072B2',"Point Forecast"="#000000"))+
  facet_wrap(.~council)+
  coord_cartesian(ylim = c(0, max(exp(fcst_sub$.mean))*2)) + 
  theme_minimal() +
  # title('Lindi Municipal Council')+
  theme(panel.border = element_rect(color = "lightgrey", fill = NA))
ggsave(paste0(region_to_filter,'_tz_arimax_forecast.png'),plot, width = 10, height = 6)
