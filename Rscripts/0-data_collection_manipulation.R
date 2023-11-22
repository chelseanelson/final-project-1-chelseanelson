### Loading Packages ----
library(tidyverse)
library(readxl)
# Data Collection ----
## larger version of dataset
fbc_data <- read_excel("data/raw/fbc_data_2022_raw.xlsx", sheet = "County") %>%
  janitor::clean_names()
fbc_data_codebook <- read_excel("data/raw/fbc_data_2022_raw.xlsx", 
                                sheet = "Codebook")
fbc_data
fbc_data_codebook

# smaller version of dataset --> only shows annual cost
cost_of_living_us <- read_csv("data/raw/cost_of_living_us_raw.csv") 
cost_of_living_us

# racial majority dataset --> joining to my fbc_data 

# Checking for NA Data ----
# skimming the data for NA 
fbc_data %>% skimr::skim()
fbc_data %>% naniar::miss_var_summary()

cost_of_living_us %>% skimr::skim()
cost_of_living_us %>% naniar::miss_var_summary()

# Data Manipulation and Wrangling ----
# adding minimum wage
fbc_data <- fbc_data %>% mutate(
  minimum_wage = case_when(
    state_abv %in% c("AL","GA","ID","IN","IA",
                     "KS","KY","LA","MS","NH","NC","ND",
                     "OK","PA","SC","TN", "TX","UT","WI","WY") ~ 7.25,
    state_abv %in% c("AR","FL","VA") ~ 11.00,
    state_abv == "AK" ~ 10.34,
    state_abv == "AZ" ~ 12.80,
    state_abv == "CA" ~ 15.00,
    state_abv == "CO" ~ 12.56,
    state_abv == "CT" ~ 14.00,
    state_abv %in% c("DE","NV") ~ 10.50,
    state_abv %in% c("HI","IL") ~ 12.00,
    state_abv == "ME" ~ 12.75,
    state_abv == "MD" ~ 12.50,
    state_abv == "MA" ~ 14.25,
    state_abv == "MI" ~ 9.87,
    state_abv == "MN" ~ 10.33,
    state_abv == "MO" ~ 11.15,
    state_abv == "MT" ~ 9.20,
    state_abv == "NE" ~ 9.00,
    state_abv == "NJ" ~ 13.00,
    state_abv == "NM" ~ 11.50, 
    # there is a different minimum wage for jobs in New York City,
    # Long Island, and Westchester
    state_abv == "NY" & county %in% 
      c("Westchester County", "New York County", 
        "Kings County", "Queens County", "Bronx County", 
        "Richmond County", "Nassau County", "Suffolk County") ~ 15.00,
    state_abv == "NY" ~ 13.20,
    state_abv == "OH" ~ 9.30,
    state_abv == "OR" & metro == 0 ~ 12.50,
    state_abv == "OR" & county %in% c("Multnomah County", "Washington County",
                                      "Clackamas County") ~ 14.75,
    state_abv == "OR" ~ 13.50,
    state_abv == "RI" ~ 12.25,
    state_abv == "SD" ~ 9.95,
    state_abv == "VT" ~ 12.55,
    state_abv == "WA" ~ 14.49,
    state_abv == "WV" ~ 8.75
  )
)

# ensuring what it looks like
fbc_data %>% 
  relocate(minimum_wage) %>%
  DT::datatable()

# adding racial majority of county 





# testing manipulations
fbc_data %>% 
  relocate(minimum_wage) %>% 
  filter(state_abv == "OR" & county == "Washington County")

fbc_data %>% 
  relocate(minimum_wage) %>% 
  filter(state_abv == "TX")

fbc_data %>% 
  relocate(minimum_wage) %>% 
  filter(state_abv == "NY" & 
           county %in% c("New York County", "Albany County"))




