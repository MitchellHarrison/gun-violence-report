library(tidyverse)

# get all suicide file names
file_names <- list.files("data/suicide", full.names = TRUE)
joined <- tibble()

# combine all mental health data into a single dataset
for (i in seq_along(file_names)) {
  data <- read_tsv(file_names[i]) |>
    janitor::clean_names() |>
    group_by(year, state) |>
    summarise(gun_suicides = sum(deaths), population = first(population))
  joined <- bind_rows(joined, data)
}

# write data to csv
joined |>
  filter(!is.na(year)) |>
  arrange("year") |>
  write_csv("data/gun_suicides.csv")