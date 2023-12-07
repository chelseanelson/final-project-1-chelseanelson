# Categorical Univariate Analysis ----

# looking at count of metro to nonmetro counties in total
figure_1_univariate_metro <- fbc_data %>% ggplot(aes(metro)) + 
  geom_bar(color = "white", fill = "black") + 
  labs(
  x = "Metro Status",
  y = "Count",
  title = "Distribution of Metro Status"
) + 
  scale_x_discrete(labels = c("0" = "Nonmetro", "1" = "Metro")) + 
  theme_light() 

ggsave("figures/univariate/figure-1.png", figure_1_univariate_metro)

# looking at count of how many times each state appears, 
# thus how many counties there are 
fbc_data %>% ggplot(aes(state_abv)) + geom_bar(color = "white")

# looking at count of where most of the data lies in terms of region
fbc_data %>% ggplot(aes(region)) + geom_bar(color = "white")

# Numerical Univariate Analysis ----
figure_2_univariate_housing_m <- fbc_data %>% ggplot(aes(housing_monthly)) + 
  geom_histogram(color = "white", binwidth = 75) +
  labs(
    title = "Distribution of Housing Monthly Costs",
    x = "Housing Monthly Costs",
    y = "Count"
  ) + 
  theme_light()


figure_3_univariate_healthcare_a <- fbc_data %>% 
  ggplot(aes(healthcare_annual)) + 
  geom_histogram(color = "white", binwidth = 1000) +
  labs(
    title = "Distribution of Healthcare Annual Costs",
    x = "Healthcare Annual Costs",
    y = "Count"
  ) + 
  theme_light()
# what other factors are playing into the healthcare annual cost?
# location, family type, racial background

figure_4_univariate_total_a <- fbc_data %>% ggplot(aes(total_annual)) +
  geom_histogram(color = "white", binwidth = 4000) +
  labs(
    title = "Distribution of Total Annual Costs",
    x = "Total Annual Costs",
    y = "Count"
  ) + 
  theme_light()

figure_5_univariate_total_m <- fbc_data %>% ggplot(aes(total_monthly)) + 
  geom_histogram(color = "white", binwidth = 400) +
  labs(
    title = "Distribution of Total Monthly Costs",
    x = "Total Monthly Costs",
    y = "Count"
  ) + 
  theme_light()

fbc_data %>% ggplot(aes(food_annual)) + 
  geom_histogram(color = "white", binwidth = 500)
# does this account for people that use food stamps or how is that intrepretated
# in the data collection

# function created to easily look at numerical distributions 
numerical_distribution_annual <- function(df, numerical_var) {
  df %>% 
    ggplot(aes( {{ numerical_var }})) + 
    geom_histogram(color = "white", binwidth = 1000) + 
    labs(
      title = paste("Distribution of", 
                    str_to_title(
                      str_replace_all(quo_name(ensym(numerical_var)), "_", " ")),
                    "Costs"),
      x = paste(str_to_title(
        str_replace_all(quo_name(ensym(numerical_var)), "_", " ")), "Costs"),
      y = "Count"
    ) + 
    theme_light()
}

numerical_distribution_monthly <- function(df, numerical_var) {
  df %>% 
    ggplot(aes( {{ numerical_var }})) + 
    geom_histogram(color = "white", binwidth = 50) + 
    labs(
      title = paste("Distribution of", 
                    str_to_title(
                      str_replace_all(quo_name(ensym(numerical_var)), "_", " ")), 
                    "Costs"),
      x = paste(str_to_title(
        str_replace_all(quo_name(ensym(numerical_var)), "_", " ")), "Costs"),
      y = "Count"
    ) + 
    theme_light()
}

figure_6_univariate_housing_a <- fbc_data %>% numerical_distribution_annual(housing_annual)
figure_7_univariate_healthcare_m <- fbc_data %>% numerical_distribution_monthly(healthcare_monthly)

figure_10_univariate_transportation_m <- fbc_data %>% numerical_distribution_monthly(transportation_monthly)
figure_11_univariate_transportation_a <- fbc_data %>% numerical_distribution_annual(transportation_annual)

fig_14_univariate_food_m <- fbc_data %>% numerical_distribution_monthly(food_monthly)
fig_15_univariate_food_a <- fbc_data %>% numerical_distribution_annual(food_annual)

# how does this compare for people who live or don't live in metropolitan 
# areas, but also within that the south metros vs the north metros

fig_16_univariate_taxes_a <- fbc_data %>% numerical_distribution_annual(taxes_annual)
fig_17_univariate_taxes_m <- fbc_data %>% numerical_distribution_monthly(taxes_monthly)

fig_18_univariate_childcare_a <- fbc_data %>% numerical_distribution_annual(childcare_annual)
fig_19_univariate_childcare_m <- fbc_data %>% numerical_distribution_monthly(childcare_monthly)

fig_20_univariate_other_necess_m <- fbc_data %>% numerical_distribution_monthly(other_necessities_monthly)
fig_21_univariate_other_necess_a <- fbc_data %>% numerical_distribution_annual(other_necessities_annual)


annual_combined_plots <- figure_3_univariate_healthcare_a /
  figure_6_univariate_housing_a / figure_4_univariate_total_a

monthly_combined_plots <- figure_7_univariate_healthcare_m / 
  figure_2_univariate_housing_m / figure_5_univariate_total_m

annual_combined_plots_2 <- figure_3_univariate_healthcare_a /
  figure_6_univariate_housing_a / figure_11_univariate_transportation_a /
figure_4_univariate_total_a

monthly_combined_plots_2 <- figure_7_univariate_healthcare_m / 
  figure_2_univariate_housing_m / figure_10_univariate_transportation_m /
  figure_5_univariate_total_m

annual_combined_plots_3 <- fig_15_univariate_food_a / fig_16_univariate_taxes_a /
  fig_18_univariate_childcare_a / fig_21_univariate_other_necess_a

monthly_combined_plots_3 <- fig_14_univariate_food_m / fig_17_univariate_taxes_m / fig_19_univariate_childcare_m / fig_20_univariate_other_necess_m


## saving plots 
ggsave("figures/univariate/figure-1.png", figure_1_univariate_metro)
ggsave("figures/univariate/figure-2.png", figure_2_univariate_housing_m)
ggsave("figures/univariate/figure-3.png", figure_3_univariate_healthcare_a)
ggsave("figures/univariate/figure-4.png", figure_4_univariate_total_a)
ggsave("figures/univariate/figure-5.png", figure_5_univariate_total_m)
ggsave("figures/univariate/figure-6.png", figure_6_univariate_housing_a)
ggsave("figures/univariate/figure-7.png", figure_7_univariate_healthcare_m)
ggsave("figures/univariate/figure-8.png", annual_combined_plots)
ggsave("figures/univariate/figure-9.png", monthly_combined_plots)
ggsave("figures/univariate/figure-10.png", figure_10_univariate_transportation_m)
ggsave("figures/univariate/figure-11.png", figure_11_univariate_transportation_a)
ggsave("figures/univariate/figure-12.png", annual_combined_plots_2)
ggsave("figures/univariate/figure-13.png", monthly_combined_plots_2)
ggsave("figures/univariate/figure-14.png", fig_14_univariate_food_m)
ggsave("figures/univariate/figure-15.png", fig_15_univariate_food_a)
ggsave("figures/univariate/figure-16.png", fig_16_univariate_taxes_a)
ggsave("figures/univariate/figure-17.png", fig_17_univariate_taxes_m)
ggsave("figures/univariate/figure-18.png", fig_18_univariate_childcare_a)
ggsave("figures/univariate/figure-19.png", fig_19_univariate_childcare_m)
ggsave("figures/univariate/figure-20.png", fig_20_univariate_other_necess_m)
ggsave("figures/univariate/figure-21.png", fig_21_univariate_other_necess_a)
ggsave("figures/univariate/figure-22.png", annual_combined_plots_3)
ggsave("figures/univariate/figure-23.png", monthly_combined_plots_3)

# Currently I am most interested in the healthcare annually, total annually, 
# total monthly, food annually, food monthly, transportation monthly, 
# transportation annually, childcare annually, childcare monthly, healthcare
# monthly

# Also I am interested in how taxes compare to other expenses that people have 

