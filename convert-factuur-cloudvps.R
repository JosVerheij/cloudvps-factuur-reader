library(tidyverse)
library(stringr)
library(lubridate)

# Set working directory
setwd("~/projects/HippoLine/hl-it")

factuur.txt <- read_delim("factuur-20180601-cloudvps-1.txt", "\n", col_names = F)

factuur.tibble <- factuur.txt %>%
  # type
  # mutate(
  #   omschrijving = str_extract(X1, "^[^-]+(?=\\s+[-]+\\s+)")
  # ) %>%
  # omschrijving
  mutate(
    omschrijving = str_extract(X1, "^.*(?=[0-9]{2}/[0-9]{2}/[0-9]{2}\\s[-]\\s)")
  ) %>%
  # type
  mutate(
    type = as.factor(ifelse(
      # licenties
      str_detect(omschrijving, regex("\\[license\\]|windows|sophos", TRUE)), "licenties", ifelse(
        # opslag
        str_detect(omschrijving, regex("\\[volume\\]|\\[image\\]|extra\\sspace", TRUE)), "opslag", ifelse(
          # systemen
          str_detect(omschrijving, regex("\\[server\\]|\\[vps[0-9]+\\]|cpu\\scores|extra\\sram", TRUE)), "systemen", "overig")
        )
    ))
  ) %>%
  # periode start
  mutate(
    periode_start = dmy(str_extract(X1, "[0-9]{2}/[0-9]{2}/[0-9]{2}"))
  ) %>%
  # periode eind
  mutate(
    periode_eind = dmy(str_extract(X1, "(?<=\\s[-]\\s)[0-9]{2}/[0-9]{2}/[0-9]{2}"))
  ) %>%
  # kosten
  mutate(kosten = as.numeric(str_extract(X1, "\\s*[0-9]+\\.[0-9]{2}\\s*$"))) %>%
  select(-X1)

# Handmatige check met CloudVPS factuur
print(sum(factuur.tibble$kosten))

factuur.tibble %>%
  group_by(type) %>%
  summarise(totaal = sum(kosten))

write.csv2(factuur.tibble, file = "factuurkosten.csv")
