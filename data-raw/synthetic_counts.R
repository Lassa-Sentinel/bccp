# Generate synthetic_counts dataset
set.seed(42)

synthetic_counts <- bccp::generate_synthetic_data(
  n = 208,  # 4 years of weekly data
  start_date = "2020-01-01",
  seasonal_amplitude = 2,
  base_rate = 3,
  pred_noise = 0.5
)

usethis::use_data(synthetic_counts, overwrite = TRUE)
