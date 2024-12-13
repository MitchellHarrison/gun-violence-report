library(tidyverse)

# get all suicide file names
file_names <- list.files("data/suicide", full.names = TRUE)
joined <- tibble()

# combine all mental health data into a single dataset
for (i in seq_along(file_names)) {
  data <- read_tsv(file_names[i])
  data <- data |>
    janitor::clean_names() |>
    filter(is.na(notes)) |>
    select(year, state, population, deaths) |>
    rename(gun_suicides = "deaths")
  joined <- bind_rows(joined, data)
}

# write data to csv
joined |>
  arrange("year") |>
  write_csv("data/gun_suicides.csv")