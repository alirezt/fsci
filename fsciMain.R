library(tidyverse)
library(labelled)

# 1. iso ----
input = "C:/Users/Apple/Desktop/DQQ paper project/fsci/Rcodes/sInputs"
iso3 = read.csv(paste(input,"/iso3.csv", sep = ""))
glimpse(iso3)


iso <- iso3 %>%
  rename(country = name) %>%
  rename(ISO = iso3) %>%
  set_variable_labels(country = "Country name", ISO = "ISO-alpha3 code") %>%

look_for(iso)

# Replacing the names of 43 countries according to the offical UN names
UNnames <- c( # old names = new names
  "Bahamas (the)" = "Bahamas",
  "British Indian Ocean Territory (the)" = "British Indian Ocean Territory",
  "Cayman Islands (the)" = "Cayman Islands",
  "Central African Republic (the)" = "Central African Republic",
  "Cocos (Keeling) Islands (the)" = "Cocos (Keeling) Islands",
  "Comoros (the)" = "Comoros",
  "Congo (the Democratic Republic of the)" = "Democratic Republic of the Congo",
  "Congo (the)" = "Congo",
  "Cook Islands (the)" = "Cook Islands",
  "Côte d'Ivoire" = "Côte D'Ivoire",
  "Dominican Republic (the)" = "Dominican Republic",
  "Falkland Islands (the) [Malvinas]" = "Falkland Islands (Malvinas)",
  "Faroe Islands (the)" = "Faroe Islands",
  "French Southern Territories (the)" = "French Southern and Antarctic Territories",
  "Gambia (the)" = "Gambia (Republic of The)",
  "Holy See (the)" = "Holy See",
  "Korea (the Democratic People's Republic of)" = "Dem People's Rep of Korea",
  "Korea (the Republic of)" = "Republic of Korea",
  "Lao People's Democratic Republic (the)" = "Lao People's Democratic Republic",
  "Macao" = "Macau",
  "Marshall Islands (the)" = "Marshall Islands",
  "Moldova (the Republic of)" = "Republic of Moldova",
  "Netherlands (the)" = "Netherlands",
  "Niger (the)" = "Niger",
  "Northern Mariana Islands (the)" = "Northern Mariana Islands",
  "Philippines (the)" = "Philippines",
  "Russian Federation (the)" = "Russian Federation",
  "Réunion" = "Reunion",
  "Saint Helena, Ascension and Tristan da Cunha" = "Saint Helena",
  "Saint Pierre and Miquelon" = "Saint Pierre et Miquelon",
  "Sudan (the)" = "Sudan",
  "Svalbard and Jan Mayen" = "Svalbard and Jan Mayen Islands",
  "Syrian Arab Republic (the)" = "Syrian Arab Republic",
  "Taiwan (Province of China)" = "Taiwan",
  "Tanzania, the United Republic of" = "United Republic of Tanzania",
  "United Arab Emirates (the)" = "United Arab Emirates",
  "United Kingdom of Great Britain and Northern Ireland (the)" = "United Kingdom of Great Britain and Northern Ireland",
  "United States Minor Outlying Islands (the)" = "United States Minor Outlying Islands",
  "United States of America (the)" = "United States of America",
  "Venezuela (Bolivarian Republic of)" = "Venezuela, Bolivarian Republic of",
  "Virgin Islands (British)" = "British Virgin Islands",
  "Virgin Islands (U.S.)" = "United States Virgin Islands",
  "Western Sahara*" = "Western Sahara"
)

# not sure why this does not work
isotest <- iso %>%
  mutate(
    country = stringr::str_replace_all(country, UNnames)
  )

# use base R for the task
i <- iso$country %in% names(UNnames)
iso$country[i] <- UNnames[iso$country[i]]

look_for(iso)
glimpse(iso)

# save as csv
write.csv(iso, "csv output/iso.csv", row.names = FALSE)

# 2. g2015_2005_1 ----
g2015 <- read.csv("csv inputs/g2015_2005_1.csv")
look_for(g2015)
glimpse(g2015)

g2015gb <- g2015 %>%
  group_by(ADM0_NAME, STATUS, DISP_AREA, ADM0_CODE) %>%
  summarise() %>%
  rename(country = ADM0_NAME) %>%
  mutate(
  # Change "Occupied Palestinan Territory" and "Antarctica"
  # and make 'status' a factor variable
    UN_status_detail = factor(case_match(STATUS,
                                         "Occupied Palestinan Territory" ~ "Occupied Palestinian Territory",
                                         "" ~ NA,
                                         .default = STATUS))
  ) %>%
  set_variable_labels(UN_status_detail = "UN status and territorial details") %>%
  mutate(
    UNmemberstate = ifelse(UN_status_detail == "Member State", 1, 0)
  ) %>%
  mutate(
    UN_status = ifelse(UNmemberstate == 1, 5,
                       ifelse(UN_status_detail == "PT Territory" | UN_status_detail == "Sovereignty unsettled", 2,
                              ifelse(UN_status_detail == "UK Territory" | UN_status_detail == "UK territory", 3,
                                     ifelse(UN_status_detail == "UK Non-Self-Governing Territory", 4,
                                            ifelse(UN_status_detail == "AU Territory" |
                                                   UN_status_detail == "CN Province" |
                                                   UN_status_detail == "CN Special Administrative Region" |
                                                   UN_status_detail == "DK Self-Governing Territory" |
                                                   UN_status_detail == "DK Territory" |
                                                   UN_status_detail == "FR Non-Self-Governing Territory" |
                                                   UN_status_detail == "FR Territory" |
                                                   UN_status_detail == "Member State" |
                                                   UN_status_detail == "NL Self-Governing Territory" |
                                                   UN_status_detail == "NO Territory" |
                                                   UN_status_detail == "NZ Non-Self-Governing Territory" |
                                                   UN_status_detail == "NZ Territory" |
                                                   UN_status_detail == "Non-Self-Governing Territory" |
                                                   UN_status_detail == "Occupied Palestinian Territory" |
                                                   UN_status_detail == "The City of Vatican" |
                                                   UN_status_detail == "US Non-Self-Governing Territory" |
                                                   UN_status_detail == "US Territory" |
                                                   UN_status_detail == "VE Territory" |
                                                   UN_status_detail == "AU Territory" |
                                                   UN_status_detail == "AU Territory", 5, 0)))))
  ) %>%
  mutate(
    territoryof = case_when(UN_status == 5 ~ STATUS)
  )
g2015gb
#levels(g2015gb$UN_status_detail)
#str(g2015gb$UN_status_detail)

UNname1 <- c(
  "Antigua & Barbuda" = "Antigua and Barbuda",
  "Bolivia" = "Bolivia (Plurinational State of)",
  "Bosnia & Herzegovina" = "Bosnia and Herzegovina",
  "Cape Verde" = "Cabo Verde",
  "CÃ´te d'Ivoire" = "Côte D'Ivoire",
  "Czech Republic" = "Czechia",
  "Swaziland" = "Eswatini",
  "Gambia" = "Gambia (Republic of The)",
  "Libyan Arab Jamahiriya" = "Libya",
  "The former Yugoslav Republic of Macedonia" = "North Macedonia",
  "Moldova, Republic of" = "Republic of Moldova",
  "Turkey" = "Türkiye",
  "U.K. of Great Britain and Northern Ireland" = "United Kingdom of Great Britain and Northern Ireland",
  "Venezuela" = "Venezuela, Bolivarian Republic of",
  "Vietnam"	= "Viet Nam",
  "RÃ©union" = "Reunion",
  "Turks and Caicos islands" = "Turks and Caicos Islands",
  "Iran  (Islamic Republic of)" = "Iran (Islamic Republic of)"
)

ii <- g2015gb$country %in% names(UNname1)
g2015gb$country[ii] <- UNname1[g2015gb$country[ii]]

UNStatus <- full_join(x = g2015gb, y = iso, "country")

UNStatus <- UNStatus %>%
  ungroup() %>%
  select(!c(STATUS, ADM0_CODE)) %>%
  filter(!is.na(DISP_AREA) | country %in% c("South Sudan", "Serbia", "Montenegro")) %>%
  mutate(
    UNmemberstate = case_when(country == "South Sudan" | country == "Serbia" | country == "Montenegro" ~ 1, .default = UNmemberstate),
    UN_status_detail = case_when(country == "South Sudan" | country == "Serbia" | country == "Montenegro" ~ "Member State", .default = UN_status_detail),
    UN_status = case_when(country == "South Sudan" | country == "Serbia" | country == "Montenegro" ~ 1, .default = UN_status),
    DISP_AREA = case_when(country == "South Sudan" | country == "Serbia" | country == "Montenegro" ~ "NO", .default = DISP_AREA)
  ) %>%
  filter(country != "Serbia and Montenegro") %>%
  arrange(country) %>%
  print(n = 300)

write.csv(x = UNStatus, file = "csv output/UNstatus.csv", row.names = FALSE)









