### Loading Packages ----
library(tidyverse)
library(readxl)
library(patchwork)
# Data Collection ----
## larger version of dataset
fbc_data <- read_excel("data/raw/fbc_data_2022_raw.xlsx", sheet = "County") %>%
  janitor::clean_names()
fbc_data_codebook_raw <- read_excel("data/raw/fbc_data_2022_raw.xlsx", 
                                sheet = "Codebook")
fbc_data
fbc_data_codebook_raw

# smaller version of dataset --> only shows annual cost
cost_of_living_us <- read_csv("data/raw/cost_of_living_us_raw.csv") 
cost_of_living_us

# racial majority dataset --> joining to my fbc_data 
# still seeing if I want to include this in my analysis 

# Checking for NA Data ----
# skimming the data for NA 
fbc_data %>% skimr::skim()
fbc_data %>% naniar::miss_var_summary()
# 5 variables have 10 missing observations 

cost_of_living_us %>% skimr::skim()
cost_of_living_us %>% naniar::miss_var_summary()

fbc_data %>% filter(is.na(median_family_income))
# These all pertains to a county in Missouri. If all of the other accounts of NA values for the other variables also pertains to this singular county,
# I will just exclude it from my analysis.

# looking at that particular case 
fbc_data %>% filter(case_id == 1533) %>% relocate(median_family_income, num_counties_in_st, st_cost_rank, st_med_aff_rank, st_income_rank)
# all accounts of NA data pertain to this one case, thus I feel it would be 
# best to remove it from my data indefinitely, by using na.omit(). 

# Data Manipulation and Wrangling ----
# adding minimum wage
# This was sourced from paycom.com (include link)
fbc_data <- na.omit(fbc_data) %>% mutate(
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
    state_abv == "WV" ~ 8.75,
    state_abv == "DC" ~ 16.10
  )
)

# ensuring what it looks like
fbc_data %>% 
  relocate(minimum_wage) %>%
  DT::datatable()

# changing metro to a factor variable with 0 = not in a metro
# 1 = in a metro

fbc_data <- fbc_data %>% 
  mutate(
    metro = as_factor(metro)
  )

# adding regional districts
# This was provided by the Census.gov (link it)
fbc_data <- fbc_data %>% 
  mutate(
    region = case_when(
      state_abv %in% c("WA","OR","CA","NV","AZ",
                       "UT","ID","MT","WY","CO","NM", "AK","HI") ~ "west",
      state_abv %in% c("ND","SD","NE","KS","MN",
                       "IA","MO","WI","IL","IN","MI","OH") ~ "midwest",
      state_abv %in% c("TX","OK","AR","LA","KY","TN","MS","AL",
                       "WV","MD","DE","VA","NC","SC","GA","FL","DC") ~ "south",
      state_abv %in% c("PA","NY","NJ","CT","RI","MA",
                       "VT","NH","ME") ~ "northeast"
    )
  )
  

# adding racial majority of county 


# ensuring missingness as been taken care of
fbc_data %>% skimr::skim()


# saving as rds to use in qmd file 
write_rds(fbc_data, "data/fbc_data")

# testing manipulations
fbc_data %>% 
  relocate(minimum_wage, region) %>% 
  filter(state_abv == "OR" & county == "Washington County")

fbc_data %>% 
  relocate(minimum_wage, region) %>% 
  filter(state_abv == "TX")

fbc_data %>% 
  relocate(minimum_wage, region) %>% 
  filter(state_abv == "NY" & 
           county %in% c("New York County", "Albany County"))




