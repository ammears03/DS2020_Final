library(tidyverse)
df <- read.csv("career_explorer_occupational_level_data_1192_rows.csv")
df |> filter(
  liccert_notes !=""
)|>View()

df |> filter(occupational_title == 'Chief Executive')|> 
  select(estimated_employment) |> unique() |> sum()

df |> filter(geography == "State of Iowa", top_skill_code == 'N.A.') |> View()

test <- read.csv("https://raw.githubusercontent.com/ammears03/DS2020_Final/refs/heads/main/career_explorer_occupational_level_data_1192_rows.csv")
