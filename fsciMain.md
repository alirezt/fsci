FSCI_Indicators
================
13/04/2023

``` r
library(tidyverse)
library(labelled)

# 1. Iso  ----
iso3 = read.csv("csv inputs/iso3.csv")
glimpse(iso3)
```

    ## Rows: 249
    ## Columns: 2
    ## $ name <chr> "Afghanistan", "Albania", "Algeria", "American Samoa", "Andorra",~
    ## $ iso3 <chr> "AFG", "ALB", "DZA", "ASM", "AND", "AGO", "AIA", "ATA", "ATG", "A~

``` r
iso <- iso3 %>%
  rename(country = name) %>%
  rename(ISO = iso3) %>%
  set_variable_labels(country = "Country name", ISO = "ISO-alpha3 code")

look_for(iso)
```

    ##  pos variable label           col_type values
    ##  1   country  Country name    chr            
    ##  2   ISO      ISO-alpha3 code chr

``` r
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

# save as csv
write.csv(iso, "csv output/iso.csv", row.names = FALSE)

# 2. UNStatus ----
g2015 <- read.csv("csv inputs/g2015_2005_1.csv")
look_for(g2015)
```

    ##  pos variable   label col_type values
    ##  1   FID        —     int            
    ##  2   ADM1_CODE  —     int            
    ##  3   ADM1_NAME  —     chr            
    ##  4   STR1_YEAR  —     int            
    ##  5   EXP1_YEAR  —     int            
    ##  6   STATUS     —     chr            
    ##  7   DISP_AREA  —     chr            
    ##  8   ADM0_CODE  —     int            
    ##  9   ADM0_NAME  —     chr            
    ##  10  Shape_Leng —     dbl            
    ##  11  Shape_Area —     dbl

``` r
glimpse(g2015)
```

    ## Rows: 3,332
    ## Columns: 11
    ## $ FID        <int> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1~
    ## $ ADM1_CODE  <int> 40542, 40543, 40544, 40545, 40546, 40547, 40548, 40549, 405~
    ## $ ADM1_NAME  <chr> "Bubanza", "Bujumbura Mairie", "Bujumbura Rural", "Bururi",~
    ## $ STR1_YEAR  <int> 1000, 1991, 1991, 1000, 1000, 1000, 1000, 1000, 1000, 1000,~
    ## $ EXP1_YEAR  <int> 3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000,~
    ## $ STATUS     <chr> "Member State", "Member State", "Member State", "Member Sta~
    ## $ DISP_AREA  <chr> "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO",~
    ## $ ADM0_CODE  <int> 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43,~
    ## $ ADM0_NAME  <chr> "Burundi", "Burundi", "Burundi", "Burundi", "Burundi", "Bur~
    ## $ Shape_Leng <dbl> 1.48815248, 0.79758716, 2.40865578, 2.89642602, 2.19801887,~
    ## $ Shape_Area <dbl> 0.085479465, 0.022629711, 0.118377601, 0.257064524, 0.15269~

``` r
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
```

    ## # A tibble: 273 x 8
    ## # Groups:   country, STATUS, DISP_AREA [273]
    ##    country   STATUS DISP_AREA ADM0_CODE UN_status_detail UNmemberstate UN_status
    ##    <chr>     <chr>  <chr>         <int> <fct>                    <dbl>     <dbl>
    ##  1 Afghanis~ "Memb~ NO                1 Member State                 1         5
    ##  2 Aksai Ch~ "Sove~ YES               2 Sovereignty uns~             0         2
    ##  3 Albania   "Memb~ NO                3 Member State                 1         5
    ##  4 Algeria   "Memb~ NO                4 Member State                 1         5
    ##  5 American~ "US N~ NO                5 US Non-Self-Gov~             0         5
    ##  6 Andorra   "Memb~ NO                7 Member State                 1         5
    ##  7 Angola    "Memb~ NO                8 Member State                 1         5
    ##  8 Anguilla  "UK N~ NO                9 UK Non-Self-Gov~             0         4
    ##  9 Antarcti~ ""     NO               10 <NA>                        NA        NA
    ## 10 Antigua ~ "Memb~ NO               11 Member State                 1         5
    ## # i 263 more rows
    ## # i 1 more variable: territoryof <chr>

``` r
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
  "Vietnam" = "Viet Nam",
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
  arrange(country)

UNStatus
```

    ## # A tibble: 275 x 7
    ##    country  DISP_AREA UN_status_detail UNmemberstate UN_status territoryof ISO  
    ##    <chr>    <chr>     <chr>                    <dbl>     <dbl> <chr>       <chr>
    ##  1 Afghani~ NO        Member State                 1         5 Member Sta~ AFG  
    ##  2 Aksai C~ YES       Sovereignty uns~             0         2 <NA>        <NA> 
    ##  3 Albania  NO        Member State                 1         5 Member Sta~ ALB  
    ##  4 Algeria  NO        Member State                 1         5 Member Sta~ DZA  
    ##  5 America~ NO        US Non-Self-Gov~             0         5 US Non-Sel~ ASM  
    ##  6 Andorra  NO        Member State                 1         5 Member Sta~ AND  
    ##  7 Angola   NO        Member State                 1         5 Member Sta~ AGO  
    ##  8 Anguilla NO        UK Non-Self-Gov~             0         4 <NA>        AIA  
    ##  9 Antarct~ NO        <NA>                        NA        NA <NA>        ATA  
    ## 10 Antigua~ NO        Member State                 1         5 Member Sta~ ATG  
    ## # i 265 more rows

``` r
write.csv(x = UNStatus, file = "csv output/UNstatus.csv", row.names = FALSE)
```
