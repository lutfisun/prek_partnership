library(here)
library(tidyverse)

board_key <- read_csv(here::here("data", "DailyRateData.csv")) %>% 
  select(Board_Number, Board_Name) %>% 
  distinct()

rate_data <- read_csv(here::here("raw_data", "2020_rates.csv")) %>% 
  dplyr::select(Board_Number = "#", provider = "Provider Type", TRS_Level = "Rating", 
                PSFT = "Pre-FT", PSBT = "Pre-BT") %>% 
  filter(provider == "LCCC" & TRS_Level != "TSR") %>% 
  pivot_longer(cols = PSFT:PSBT, names_to = "rate_class", values_to = "daily_rate") %>% 
  mutate(TRS_Level = case_when(TRS_Level == "Reg" ~ "Regular", 
                               T ~ TRS_Level)) %>% 
  left_join(board_key) %>% 
  select(Board_Number, Board_Name, TRS_Level, rate_class, daily_rate) %>% 
  write_csv(here::here("data", "DailyRateData.csv"))

