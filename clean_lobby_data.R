library(tidyverse)

lobby <- arrow::read_parquet("data/lobbyists.parquet")
gun_groups <- lobby |>
  filter(str_detect(client_name, regex(gun_filter, ignore_case = TRUE))) |>
  select(year, client_name, position, state) |>
  rename(state_id = "state") |>
  mutate(state = state.name[match(state_id, state.abb)]) |>
  select(-state_id)

all_groups <- unique(gun_groups$client_name)
anti_gun_group_ids <- c(138, 139, 140, 214:226, 238:247, 264, 303, 309, 316,
                        326, 754, 758, 765, 770, 810, 829, 951, 965, 1108,
                        1137, 1359, 1548)
anti_gun_group_str <- paste(
  "everytown", "prevent", "prevent", "stop", "safety", "gundersen",
  "gun sense", "god before guns", "gun violence task force", "against",
  "george gund", "students for gun legislation", "bbq", "gunn", "laura gundy",
  "lloyd gunker", "gunter", "moms demand action", "ray pagun", "gund",
  "city of rifle", "giffords", "illegal guns", "colorado faith", 
  "hunters against", "students demand action", "justice", "epidemic",
  "for gun control", "mayor", "home loan", "gunnarson", "gun free", 
  "gun sense", "laguna", "oliver smith", "gundry", "niu school shooting",
  "olaide", "niu mass shooting", "shooters bar",
  sep = "|"
)
anti_gun_group_names <- all_groups[anti_gun_group_ids]

gun_groups <- gun_groups |>
  filter(
    !str_detect(client_name, regex(anti_gun_group_str, ignore_case = T)),
    !(client_name %in% anti_gun_group_names)
  )

gun_group_count <- gun_groups |>
  group_by(year, state) |>
  summarise(lobbyist_positions = n(), .groups = "drop")

write_csv(gun_group_count, "gun_interest.csv")
