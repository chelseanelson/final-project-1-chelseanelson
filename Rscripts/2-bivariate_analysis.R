# Functions ----

# function for looking at distribution of the numerical variables based on
# categorical variables 
numerical_family_type <- function(numerical_var) {

  fbc_data %>% 
    ggplot(aes({{ numerical_var }})) + geom_histogram(color = "white") + 
    facet_wrap(~family) + 
    labs(
      title = paste("Distribution of", 
                    str_replace_all(quo_name(ensym(numerical_var)), "_", " "), 
                    "costs based on family type"),
      x = paste(str_to_title(
        str_replace_all(quo_name(ensym(numerical_var)), "_", " ")), "Costs"),
      y = "Count"
    ) + 
    theme_light()
  
}

numerical_metro <- function(numerical_var) {
  
  fbc_data %>% 
    ggplot(aes({{ numerical_var }})) + geom_histogram(color = "white") + 
    facet_wrap(~metro, 
               labeller = labeller(
                 metro = c("0" = "Nonmetro Areas", "1" = "Metro Areas"))) +
    labs(
      title = paste("Distribution of", 
                    str_replace_all(quo_name(ensym(numerical_var)), "_", " "), 
                    "costs for metros vs. nonmetros"),
      x = paste(str_to_title(
        str_replace_all(quo_name(ensym(numerical_var)), "_", " ")), "Costs"),
      y = "Count"
    ) + 
    theme_light()
  
}

numerical_regional <- function(numerical_var) {
  
  fbc_data %>% 
    ggplot(aes({{ numerical_var }})) + geom_histogram(color = "white") + 
    facet_wrap(~region) +
    labs(
      title = paste("Distribution of", 
                    str_replace_all(quo_name(ensym(numerical_var)), "_", " "), 
                    "costs based on geographical region"),
      x = paste(str_to_title(
        str_replace_all(quo_name(ensym(numerical_var)), "_", " ")), "Costs"),
      y = "Count"
    ) + 
    theme_light()
  
}

# Family Type ----

numerical_family_type(total_annual)*
numerical_family_type(healthcare_annual)*
numerical_family_type(food_annual)
numerical_family_type(total_monthly)*
numerical_family_type(food_monthly)
numerical_family_type(transportation_monthly)
# interesting that transportation is pretty similar across the board is this 
# because a lot of people have cars 
numerical_family_type(transportation_annual)
# same with transportation annually it is pretty similar across the board 
numerical_family_type(childcare_annual)
numerical_family_type(childcare_monthly)
# the average cost of childcare is also pretty similar across the board as well
# on both the annual and monthly levels
numerical_family_type(healthcare_monthly)*

# Metropolitan -----

numerical_metro(total_annual)*
numerical_metro(healthcare_annual)*
numerical_metro(food_annual)
numerical_metro(total_monthly)*
numerical_metro(food_monthly)
numerical_metro(transportation_monthly)
numerical_metro(transportation_annual)
numerical_metro(childcare_annual)
numerical_metro(childcare_monthly)
numerical_metro(healthcare_monthly)*


# It is interesting that on the nation level there is not a big different 
# between counties that are and aren't in a metropolitan area in terms of the 
# average cost for all of the main aspects of expenses of life 

# Regional ----

numerical_regional(total_annual)*
numerical_regional(healthcare_annual)*
numerical_regional(food_annual)
numerical_regional(total_monthly)*
numerical_regional(food_monthly)
numerical_regional(transportation_monthly)
numerical_regional(transportation_annual)
numerical_regional(childcare_annual)
numerical_regional(childcare_monthly)
numerical_regional(healthcare_monthly)*


# At the national level there seems to be little to no differences in the 
# distributions for the different categories based on the geographical region
# of the county. 

# Thus through this, we realize that it would be best to pick certain states 
# and see how we can expand this on the state level and then compare states 
# to each other. 

# Correlations and Relationships

# I want to look at how median family income correlates to total annual costs 
# on the state, regional, and metro levels

# Racial Majority ----

# I want to look at how the ranks correspond to the racial majority of 
# each state