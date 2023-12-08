# original codebook:
fbc_data_codebook_raw

# creating my own codebook: 
fbc_data_codebook <- tibble(
  column_variable = c("case_id","state_abv","metro","areaname","county", "family","housing_monthly","food_monthly","transportation_monthly","healthcare_monthly","other_necessities_monthly","childcare_monthly","taxes_monthly","total_monthly",
"housing_annual","food_annual","transportation_annual","healthcare_annual","other_necessities_annual","childcare_annual","taxes_annual","total_annual",
"median_family_income", "num_counties_in_st", "st_cost_rank","st_med_aff_rank","st_income_rank", "minimum_wage","region"),
  description = c("Used to uniquely identify observations", "Two letter state abbreviation", "Indicates whether or not a county is in a metro area, 0 = nonmetro, 1 = metro", "Name of metro area county is in (if applicable)", "County name", "Family type", "Average  monthly cost of housing", "Average monthly cost of food", "Average monthly of transportation", "Average monthly cost of healthcare", "Average monthly cost of other necessities", "Average monthly cost of childcare", "Average monthly taxes", "Average monthly total cost", "Average annual cost of housing", "Average annual cost of food", "Average annual cost of transportation", "Average annual cost of healthcare", "Average annual cost of other necessities", "Average annual cost of childcare", "Average annual taxes","Average annual total cost", "County median family income from the ACS", "The total number of counties in the state" , "Total cost rank: Ranks counties by total budget cost (1 = highest total) for each state", "\"Affordability\" rank: Ranks counties by median family income as a share of total annual cost. (1 = lowest share or least \"affordable\") for each state", "Ranks counties by median family income. (1 = highest income) for each state", "State Minmum wage from Paycom.com", "Geographical Region (Northeast, South, West, Midwest)")
)

fbc_data_codebook

# saving it 
write_csv(fbc_data_codebook, "data/fbc_data_codebook.csv")
fbc_data_codebook <- read_csv("data/fbc_data_codebook.csv")