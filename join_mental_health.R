library(tidyverse)
library(haven)

# get all mental health file names
file_names <- list.files("data/mental_health", full.names = TRUE)
joined <- tibble()

# combine all mental health data into a single dataset
for (i in seq_along(file_names)) {
  data <- read_xpt(file_names[i]) |>
    mutate(year = 1992 + i) |>
    select(year, `_STATE`, matches("MENTHLTH"), matches("VETERAN"))
  joined <- bind_rows(joined, data)
}

output <- joined_data |>
  filter(`_STATE` < 60) |> # remove PR, USVI, and Guam
  mutate(
    state = usmap::fips_info(`_STATE`)$full,
    VETERAN = case_when(
      VETERAN == 1 ~ "Yes",
      VETERAN == 2 ~ "No", 
      is.na(VETERAN) ~ NA,
      TRUE ~ "Other"
    ),
    VETERAN1 = case_when(
      VETERAN1 == 1 ~ "Yes",
      VETERAN1 == 2 ~ "No", 
      is.na(VETERAN1) ~ NA,
      TRUE ~ "Other"
    ),
    VETERAN2 = case_when(
      VETERAN2 < 3 ~ "Yes",
      VETERAN2 < 5 ~ "No", 
      is.na(VETERAN2) ~ NA,
      TRUE ~ "Other"
    ),
    VETERAN3 = case_when(
      VETERAN3 == 1 ~ "Yes",
      VETERAN3 == 2 ~ "No", 
      is.na(VETERAN3) ~ NA,
      TRUE ~ "Other"
    ),
    veteran = coalesce(VETERAN, VETERAN1, VETERAN2, VETERAN3)
  ) |>
  rename(mental_health = "MENTHLTH") |>
  select(year, state, mental_health, veteran)

write_csv(output, "data/mental_health.csv")
