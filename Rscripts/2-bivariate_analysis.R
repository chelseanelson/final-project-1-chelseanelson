# Functions ----

# function for looking at distribution of the numerical variables based on
# categorical variables 
numerical_family_type <- function(numerical_var) {

  fbc_data %>% 
    ggplot(aes({{ numerical_var }})) + geom_histogram(color = "white") + 
    facet_wrap(~family) + 
    labs(
      title = paste("Distribution of", 
                    str_to_title(str_replace_all(quo_name(ensym(numerical_var)), "_", " ")), 
                    "Costs Based on Family Type"),
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
                    str_to_title(str_replace_all(quo_name(ensym(numerical_var)), "_", " ")), 
                    "Costs Based on Metro Classification"),
      x = paste(str_to_title(
        str_replace_all(quo_name(ensym(numerical_var)), "_", " ")), "Costs"),
      y = "Count"
    ) + 
    theme_light()
  
}

numerical_regional <- function(numerical_var) {
  
  fbc_data %>% 
    ggplot(aes({{ numerical_var }})) + geom_histogram(color = "white") + 
    facet_wrap(~region,
               labeller = labeller(
                 region = c("south" = "South", "west" = "West", "northeast" = "Northeast", "midwest" = "Midwest"))) +
    labs(
      title = paste("Distribution of", 
                    str_to_title(str_replace_all(quo_name(ensym(numerical_var)), "_", " ")), 
                    "Costs Based on Geographical Region"),
      x = paste(str_to_title(
        str_replace_all(quo_name(ensym(numerical_var)), "_", " ")), "Costs"),
      y = "Count"
    ) + 
    theme_light()
  
}

# Family Type ----

figure_1_bivariate_family_total_a <- numerical_family_type(total_annual)
figure_2_bivariate_family_total_m <- numerical_family_type(total_monthly)

figure_3_bivariate_family_healthcare_a <- numerical_family_type(healthcare_annual)
figure_4_bivariate_family_healthcare_m <- numerical_family_type(healthcare_monthly)

figure_5_bivariate_family_transportation_m <- numerical_family_type(transportation_monthly)
# interesting that transportation is pretty similar across the board is this 
# because a lot of people have cars 
figure_6_bivariate_family_transportation_a <- numerical_family_type(transportation_annual)
# same with transportation annually it is pretty similar across the board

figure_7_bivariate_family_housing_a <- numerical_family_type(housing_annual)
figure_8_bivariate_family_housing_m <- numerical_family_type(housing_monthly)

numerical_family_type(food_annual)
numerical_family_type(food_monthly)
numerical_family_type(childcare_annual)
numerical_family_type(childcare_monthly)
# the average cost of childcare is also pretty similar across the board as well
# on both the annual and monthly levels
numerical_family_type(other_necessities_monthly)
numerical_family_type(other_necessities_annual)
numerical_family_type(taxes_annual)
numerical_family_type(taxes_monthly)

# Metropolitan -----

figure_9_bivariate_metro_total_a  <- numerical_metro(total_annual)
figure_10_bivariate_metro_total_m <- numerical_metro(total_monthly)

figure_11_bivariate_metro_healthcare_a <- numerical_metro(healthcare_annual)
figure_12_bivariate_metro_healthcare_m <- numerical_metro(healthcare_monthly)

figure_13_bivariate_metro_transportation_a <- numerical_metro(transportation_annual)
figure_14_bivariate_metro_transportation_m <- numerical_metro(transportation_monthly)

figure_15_bivariate_metro_housing_a <- numerical_metro(housing_annual)
figure_16_bivariate_metro_housing_m <- numerical_metro(housing_monthly)

numerical_metro(food_annual)
numerical_metro(food_monthly)
numerical_metro(childcare_annual)
numerical_metro(childcare_monthly)
numerical_metro(other_necessities_annual)
numerical_metro(other_necessities_monthly)
numerical_metro(taxes_annual)
numerical_metro(taxes_monthly)

# It is interesting that on the nation level there is not a big different 
# between counties that are and aren't in a metropolitan area in terms of the 
# average cost for all of the main aspects of expenses of life 

# Regional ----

figure_17_bivariate_regional_total_a <- numerical_regional(total_annual)
figure_18_bivariate_regional_total_m <- numerical_regional(total_monthly)

numerical_regional(healthcare_annual)
numerical_regional(healthcare_monthly)

numerical_regional(transportation_annual)
numerical_regional(transportation_monthly)

numerical_regional(housing_annual)
numerical_regional(housing_monthly)

numerical_regional(food_annual)
numerical_regional(food_monthly)
numerical_regional(childcare_annual)
numerical_regional(childcare_monthly)
numerical_regional(healthcare_monthly)


total_annual_combined_plots_bi <- figure_3_bivariate_metro_total_a / figure_5_bivariate_regional_total_a

total_monthly_combined_plots_bi <- figure_4_bivariate_metro_total_m / figure_6_bivariate_regional_total_m




# saving plots 
ggsave("figures/bivariate/figure-1_bi.png", figure_1_bivariate_family_total_a)
ggsave("figures/bivariate/figure-2_bi.png", figure_2_bivariate_family_total_m)
ggsave("figures/bivariate/figure-3_bi.png", figure_3_bivariate_metro_total_a)
ggsave("figures/bivariate/figure-4_bi.png", figure_4_bivariate_metro_total_m)
ggsave("figures/bivariate/figure-5_bi.png", figure_5_bivariate_regional_total_a)
ggsave("figures/bivariate/figure-6_bi.png", figure_6_bivariate_regional_total_m)
ggsave("figures/bivariate/figure-7_bi.png", total_annual_combined_plots_bi)
ggsave("figures/bivariate/figure-8_bi.png", total_monthly_combined_plots_bi)




# At the national level there seems to be little to no differences in the 
# distributions for the different categories based on the geographical region
# of the county. 

# Thus through this, we realize that it would be best to pick certain states 
# and see how we can expand this on the state level and then compare states 
# to each other. 

# Correlations and Relationships

# I want to look at how median family income correlates to total annual costs 
# on the state, regional, and metro levels

# national level correlations 
correlation_matrix <- fbc_data %>% 
  select(ends_with("_annual"),ends_with("_monthly"), minimum_wage, median_family_income, st_income_rank) %>% 
  cor() 

cor_names <- c(
  "housing - annual", "food - annual", "transportation - annual", 
  "healthcare - annual", "other necessities - annual", "childcare - annual", 
  "taxes - annual", "total - annual", "housing - monthly", "food - monthly",
  "transportation - monthly", "healthcare - monthly", 
  "other necessities - monthly", "childcare - monthly", "taxes - monthly",
  "total - monthly", "minimum wage", "median family income", 
  "in-state income rank"
)

colnames(correlation_matrix) <- rownames(correlation_matrix) <- cor_names

correlation_plot <- ggcorrplot(correlation_matrix, lab = TRUE, type = "lower") + 
  labs(
    title = "Cost of Living Correlations"
  )

ggsave("figures/bivariate/correlation-matrix_national.png", correlation_plot)
