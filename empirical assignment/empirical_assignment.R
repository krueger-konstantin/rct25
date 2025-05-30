### Empirical Assignment ###

# Loading required libraries
library(tidyverse)
library(stringdist)
library(lubridate)
library(broom)
library(knitr)
library(kableExtra)
library(stringr)

# ============================================
#               ### Step 3 ###               
# ============================================

# Load Orbis dataset
step3_orbis_raw <- readRDS("~/RStudio/rct25/data/generated/orbis_panel_berlin.rds")

# Filter for target firms using lowercase name matching
step3_orbis_filtered <- step3_orbis_raw %>%
  filter(
    (str_detect(str_to_lower(name_internat), "gls") & postcode == "10435") |
      str_detect(str_to_lower(name_internat), "prater garten") |
      str_detect(str_to_lower(name_internat), "quandoo") |
      str_detect(str_to_lower(name_internat), "kauf dich") |
      str_detect(str_to_lower(name_internat), "bd apartment") |
      str_detect(str_to_lower(name_internat), "stayery")
  )

# Group by firm name and sort by year
step3_orbis_filtered_sorted <- step3_orbis_filtered %>%
  group_by(name_internat) %>%
  arrange(year, .by_group = TRUE)

# Quick check of the result
print(step3_orbis_filtered_sorted)

# Export result as CSV
write_csv(step3_orbis_filtered_sorted, "~/RStudio/rct25/output/step3_orbis_filtered_sorted.csv")

# ============================================
#               ### Step 4 ###               
# ============================================

# Filtering for firms in postal code 10435, year 2021
step4_orbis_10435_2021 <- step3_orbis_raw %>%
  filter(postcode == 10435, year == 2021)

# Get the top firm by total assets
step4_top1_firm <- step4_orbis_10435_2021 %>%
  filter(!is.na(toas)) %>%
  arrange(desc(toas)) %>%
  slice(1)

# Displaying task financials of top firm
step4_top1_firm %>%
  select(name_internat, postcode, year, toas, shfd, turn)

# Export result as CSV
write_csv(step4_top1_firm, "~/RStudio/rct25/output/step4_top1_firm.csv")

# Get the second top firm by total assets
step4_top2_firm <- step4_orbis_10435_2021 %>%
  filter(!is.na(toas)) %>%
  arrange(desc(toas)) %>%
  slice(2)

# Displaying task financials of second top firm
step4_top2_firm %>%
  select(name_internat, postcode, year, toas, shfd, turn)

# Export result as CSV
write_csv(step4_top2_firm, "~/RStudio/rct25/output/step4_top2_firm.csv")

# ============================================
#               ### Step 5 ###               
# ============================================

# Load and preprocess data
# Load and preprocess data
step5_firms_10435 <- readRDS("~/RStudio/rct25/data/generated/orbis_panel_berlin.rds") %>%
  filter(postcode == 10435, year == 2021) %>%
  mutate(equity_ratio = shfd / toas)

step5_firms_berlin <- readRDS("~/RStudio/rct25/data/generated/orbis_panel_berlin.rds") %>%
  filter(city_native == "Berlin", year == 2021) %>%
  mutate(equity_ratio = shfd / toas)

# Combine for t-tests
step5_firms_combined <- bind_rows(
  step5_firms_10435 %>% mutate(group = "10435"),
  step5_firms_berlin %>% mutate(group = "Berlin")
)

# Summary stats function
step5_get_stats <- function(df, var) {
  values <- df[[var]]
  tibble(
    Mean = mean(values, na.rm = TRUE),
    SD = sd(values, na.rm = TRUE),
    Min = min(values, na.rm = TRUE),
    Median = median(values, na.rm = TRUE),
    Max = max(values, na.rm = TRUE),
    IQR = IQR(values, na.rm = TRUE)
  )
}

# Get stats
step5_toas_10435 <- step5_get_stats(step5_firms_10435, "toas")
step5_toas_berlin <- step5_get_stats(step5_firms_berlin, "toas")
step5_equity_10435 <- step5_get_stats(step5_firms_10435, "equity_ratio")
step5_equity_berlin <- step5_get_stats(step5_firms_berlin, "equity_ratio")

# Run t-tests
step5_ttest_toas <- t.test(toas ~ group, data = step5_firms_combined)
step5_ttest_equity <- t.test(equity_ratio ~ group, data = step5_firms_combined)

# Formatting function
format_stats <- function(val, digits = 0, big.mark = ",") {
  ifelse(
    is.na(val), "", 
    formatC(val, digits = digits, format = "f", big.mark = big.mark)
  )
}

# Build final table (use valid R names)
step5_results_table <- tibble(
  Statistic = c("Mean", "Standard Deviation", "Minimum", "Median", "Maximum", "Interquartile Range", 
                "p-value", "95% CI Lower", "95% CI Upper"),
  
  toas_10435 = c(
    format_stats(step5_toas_10435$Mean),
    format_stats(step5_toas_10435$SD),
    format_stats(step5_toas_10435$Min),
    format_stats(step5_toas_10435$Median),
    format_stats(step5_toas_10435$Max),
    format_stats(step5_toas_10435$IQR),
    format_stats(step5_ttest_toas$p.value, digits = 8),
    format_stats(step5_ttest_toas$conf.int[1]),
    format_stats(step5_ttest_toas$conf.int[2])
  ),
  
  toas_berlin = c(
    format_stats(step5_toas_berlin$Mean),
    format_stats(step5_toas_berlin$SD),
    format_stats(step5_toas_berlin$Min),
    format_stats(step5_toas_berlin$Median),
    format_stats(step5_toas_berlin$Max),
    format_stats(step5_toas_berlin$IQR),
    format_stats(step5_ttest_toas$p.value, digits = 8),
    format_stats(step5_ttest_toas$conf.int[1]),
    format_stats(step5_ttest_toas$conf.int[2])
  ),
  
  equity_10435 = c(
    format_stats(step5_equity_10435$Mean, 2),
    format_stats(step5_equity_10435$SD, 2),
    format_stats(step5_equity_10435$Min, 2),
    format_stats(step5_equity_10435$Median, 2),
    format_stats(step5_equity_10435$Max, 2),
    format_stats(step5_equity_10435$IQR, 2),
    format_stats(step5_ttest_equity$p.value, 4),
    format_stats(step5_ttest_equity$conf.int[1], 2),
    format_stats(step5_ttest_equity$conf.int[2], 2)
  ),
  
  equity_berlin = c(
    format_stats(step5_equity_berlin$Mean, 2),
    format_stats(step5_equity_berlin$SD, 2),
    format_stats(step5_equity_berlin$Min, 2),
    format_stats(step5_equity_berlin$Median, 2),
    format_stats(step5_equity_berlin$Max, 2),
    format_stats(step5_equity_berlin$IQR, 2),
    format_stats(step5_ttest_equity$p.value, 4),
    format_stats(step5_ttest_equity$conf.int[1], 2),
    format_stats(step5_ttest_equity$conf.int[2], 2)
  )
)

# Sample sizes
n_10435 <- nrow(step5_firms_10435)
n_berlin <- nrow(step5_firms_berlin)

# Build and render the final table
step5_results_table %>%
  kable(
    caption = "Summary Statistics and Mean Comparison of Total Assets (in €) and Equity Ratio for Firms in Berlin and Postal Code 10435 (2021)",
    col.names = c("Statistic", "10435", "Berlin", "10435", "Berlin"),
    align = c("l", "c", "c", "c", "c"),
    booktabs = TRUE,
    escape = TRUE  
  ) %>%
  
  # Row 1 (top): sample sizes
  add_header_above(setNames(
    rep(1, 5),
    c(" ",
      paste0("n = ", n_10435),
      paste0("n = ", n_berlin),
      paste0("n = ", n_10435),
      paste0("n = ", n_berlin))
  )) %>%
  
  # Row 2: Variable group labels
  add_header_above(c(" " = 1, "Total Assets (€)" = 2, "Equity Ratio (%)" = 2)) %>%
  
  # Style and formatting
  kable_styling(full_width = FALSE, position = "center") %>%
  column_spec(2:3, width = "2.75cm") %>%
  column_spec(4:5, width = "2.75cm") %>%
  
  # Footnotes
  footnote(
    general = c(
      "This Orbis dataset is provided by WRDS. The sample covers the period of 2021.",
      "Firms with total assets of zero were excluded. Total assets are reported in euros.",
      "The equity ratio is defined as book value of equity divided by total assets and expressed as a percentage.",
      "Mean comparisons between firms in postal code 10435 and the overall Berlin sample are based on t-tests.",
      "Corresponding p-values indicate statistical significance."
    ),
    general_title = "Notes:"
  )