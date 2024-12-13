library(tidyverse)

# load mental health and gun suicide data
mntl <- read_csv("data/mental_health.csv")
deaths <- read_csv("data/gun_suicides.csv")

# get mean proportion of suicides that were by gun
owner <- readxl::read_excel(
    "data/gun_ownership.xlsx",
    sheet = "State-Level Data & Factor Score"
  ) |>
  janitor::clean_names() |>
  select(year, state, fem_fs_s, male_fs_s) |>
  mutate(mean_fs_s = (fem_fs_s + male_fs_s) / 2)

# classify poor mental health
mntl_annual <- mntl |>
  mutate(
    mental_health = case_when(
      mental_health == 88 ~ 0,
      mental_health > 31 ~ NA,
      TRUE ~ mental_health
    ),
    # CDC defines 15 or more days per month of poor mental state as problematic
    poor_mental = if_else(mental_health > 14, 1, 0)
  ) |>
  group_by(year, state) |>
  summarise(p_poor_mental = mean(poor_mental, na.rm = TRUE))

# join all data together into a single dataset
joined_data <- mntl_annual |>
  left_join(deaths) |>
  left_join(owner) |>
  filter(year < 2023)

write_csv(joined_data, "data/total_death_data.csv")