## Loading Packages ----
library(tidyverse)

## Question of Interest ----
#How does the cost of living vary across different geographical facets and metro classifications? 

# For myself: first look at the largest level so national, then regional, then the states in every region comparing them. followup question: Are there observable trends in transportation costs based on the availability and accessibility of public transportation in metro areas?
  
# Our first inquiry delves understanding how the geographical tapestry and metro classifications of different areas in America help to uncover the intricate variations in the cost of living. Thus through this question, I aim to unravel the economic nuances that define household budgets. 

### Follow-Up Questions -----
# Are there notable disparities in housing, transportation, or 
# healthcare costs among regions?

# Do families in metro areas face different challenges in meeting their 
# budgets compared to those in non-metro areas?

# Analyze how the metro classification interacts with family types, minimum 
# wage, and the total cost rank.

# Explore how income disparities within regions and metro classifications 
# correlate with the average cost of living.

## Start of Analysis ----

# looks at the average cost of living by region and metro classification
avg_cost_of_living_region_metro <- fbc_data %>% summarise(
  avg_cost_of_living = mean(total_annual),
  .by = c(region, metro)) %>% 
  ggplot(aes(region, avg_cost_of_living, fill = metro)) + 
  geom_col(position = "dodge") +
  geom_text(aes(label = sprintf("$%.2f", avg_cost_of_living)), position = position_dodge(width = .95), vjust = -.25, size = 3) + 
  labs(
    title = "Average Cost of Living By Metro Classification and Region",
    x = "Region",
    y = "Average Cost of Living",
    fill = "Metro\nClassification"
  ) + theme_light() + 
  scale_fill_discrete(labels = c("Nonmetro Area", "Metro Area")) + 
  scale_x_discrete(labels = c("South", "West", "Northeast","Midwest"))

ggsave("figures/multivariate/question1/question1_figure1.png", avg_cost_of_living_region_metro)

# We can see from this plot that people in the metro northeast have the 
# highest average cost of living, whereas living in the metro south has
# the extremely lower average cost of living, being also similar to the 
# cost of living in the non-metro northeast. A similar relationship is seen
# with the non metro west, which seems to be more expensive than both the metro midwest and metro south. 

# What could be the possible reasons behind this?
# infrastructure and accessibility, public transportation, housing market demands, taxes and local policies, 

# could figure out which category gets the most allocation based on region? and then divide more into the actual states.

# looks at income disparities and cost of living by region and metro classification
income_disparities <- fbc_data %>% 
  summarise(
    avg_cost_of_living = mean(total_annual),
    median_income = median(median_family_income),
    .by = c(region, metro)
  ) %>% 
  ggplot(aes(median_income, 
             avg_cost_of_living, color = region, shape = metro)) + 
  geom_point() + 
  labs(title = "Income Disparities and Cost of Living by Region and\nMetro Classification",
       x = "Median Family Income",
       y = "Average Cost of Living",
       color = "Region",
       shape = "Metro Classification") +
  theme_light() +
  scale_color_discrete(labels = c("South", "West", "Northeast", "Midwest")) +
  scale_shape_discrete(labels = c("Non-Metro Area", "Metro Area"))

ggsave("figures/multivariate/question1/question1_figure2.png", 
       income_disparities)

# from this plot we see that it would be interesting to look further into the # metro south, non-metro northeast, metro midwest, metro and non-metro west

# again we see the same relationship as before, but now interestingly 
# we see that the people in the west have extremely bad ratio between
# the amount of money they make and the actual average cost of living in 
# the west. 

# why is this? 
# housing market, regional economic disparities (certain areas may be more affluent than others), cost of healthcare, income inequalities

# on the flip side we see that people in the midwest and metro northeast make more money than they spend on average, being the only groups in this position
# as everyone else makes less money than they have to spend on average. Is
# this related to having the best access to public transport, regional policies. 

# looking at the nonmetro and metro west average distribution of spending in categories
distribution_of_spending_west_metro_region <- fbc_data %>% 
  filter(region == "west") %>%
  select(ends_with("_annual"), metro) %>% 
  summarise(
    avg_housing_annual = mean(housing_annual),
    avg_food_annual = mean(food_annual),
    avg_transportation_annual = mean(transportation_annual),
    avg_healthcare_annual = mean(healthcare_annual),
    avg_other_necessities_annual = mean(other_necessities_annual),
    avg_childcare_annual = mean(childcare_annual),
    avg_taxes_annual = mean(taxes_annual),
    avg_total_annual = mean(total_annual),
    .by = c(metro)
  ) %>%
  pivot_longer(cols = -metro, 
               names_to = "budget_component", values_to = "cost") %>%
  ggplot(aes(budget_component,cost)) + geom_col(fill = "black") + 
  geom_text(aes(label = sprintf("$%.2f", cost)), size = 2, 
            position = position_dodge(width = 0.9), vjust = -0.25) + 
  facet_grid(~metro, labeller = as_labeller(c("0" = "Nonmetro", "1" = "Metro"))) +
  theme_light() + scale_x_discrete(labels = c("childcare","food", "healthcare", "housing","other necessities","taxes","total","transportation")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(
    title = "Distribution of Spending in the Nonmetro and Metro West",
    x = "Budget Components",
    y = "Average Expense")

ggsave("figures/multivariate/question1/question1_figure3.png", distribution_of_spending_west_metro_region)

# we see here out of total spending housing is highest cost when living in a metro area with transportation being a close second, whereas when living in 
# a nonmetro area transportation is the largest expense with healthcare and housing being close behind. This is interesting, because healthcare is not one of the high expenses when living in a metro area but is when living in a nonmetro area. Also when comparing the total annual expenses, the people who live a metro area spend around $1000 a year. This could also be because people in the nonmetro have larger families, so I would have to also look at this on the family levels as well. There is also more competition between plans in metro areas thus, making companies offer lower plans to get the most busy, this is uncommon in nonmetro areas. 

dist_of_spending_west_fm <- fbc_data %>% filter(region == "west") %>%
  select(ends_with("_annual"), metro,family) %>% 
  summarise(
    avg_housing_annual = mean(housing_annual),
    avg_food_annual = mean(food_annual),
    avg_transportation_annual = mean(transportation_annual),
    avg_healthcare_annual = mean(healthcare_annual),
    avg_other_necessities_annual = mean(other_necessities_annual),
    avg_childcare_annual = mean(childcare_annual),
    avg_taxes_annual = mean(taxes_annual),
    avg_total_annual = mean(total_annual),
    .by = c(metro,family)
  ) %>%
  pivot_longer(cols = -c(metro,family), 
               names_to = "budget_component", values_to = "cost") %>%
  ggplot(aes(budget_component,cost, fill = metro)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~family) +
  theme_light() + 
  scale_x_discrete(labels = c("childcare","food", "healthcare","housing","other necessities","taxes","total","transportation")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(
    title = "Distribution of Family Size Spending in the Nonmetro and Metro West",
    x = "Budget Components",
    y = "Average Expense",
    fill = "Metro Classification") + 
  scale_fill_discrete(labels = c("Nonmetro","Metro"))

ggsave("figures/multivariate/question1/question1_figure4.png", dist_of_spending_west_fm)

# even at the family level, people who live in the west nonmetro areas seem to on average spend more money on healthcare than that of people in the metro areas. Thus this probably pertains to..... (finish this statement)
# Another interesting result that was found from this is that transporation is slightly allocated a little bit more of people's total expenses in the nonmetro areas than that of in the metro areas. This makes sense as there are buses and public transportation that is accessible in the metro areas that isn't in nonmetro areas, and that people in the west love to use cars still regardless. In all other aspects regardless of family size, people in metro areas seem to spend more money than people in nonmetro areas, which makes sense in terms of metro areas being relatively more expense than nonmetro areas for the most part in America.

# I will be turning the two plots I made just now into functions to be used for all regions. 

# function to look at the nonmetro and metro of a region's average distribution of spending in categories
distribution_of_spending_metro_region <- function(region_name) {
fbc_data %>% filter(region == region_name) %>%
select(ends_with("_annual"), metro) %>% 
  summarise(
    avg_housing_annual = mean(housing_annual),
    avg_food_annual = mean(food_annual),
    avg_transportation_annual = mean(transportation_annual),
    avg_healthcare_annual = mean(healthcare_annual),
    avg_other_necessities_annual = mean(other_necessities_annual),
    avg_childcare_annual = mean(childcare_annual),
    avg_taxes_annual = mean(taxes_annual),
    avg_total_annual = mean(total_annual),
    .by = c(metro)
  ) %>%
  pivot_longer(cols = -metro, 
               names_to = "budget_component", values_to = "cost") %>%
  ggplot(aes(budget_component,cost)) + 
    geom_text(aes(label = sprintf("$%.2f", cost)), size = 2, 
              position = position_dodge(width = 0.9), vjust = -0.25) +
    geom_col(fill = "black") + 
    facet_grid(~metro, labeller = as_labeller(c("0" = "Nonmetro", "1" = "Metro"))) +
  theme_light() + 
    scale_x_discrete(labels = c("childcare","food", "healthcare","housing","other necessities","taxes","total","transportation")) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(
      title = paste("Distribution of Spending in the Nonmetro and Metro",
                str_to_title(region_name)),
      x = "Budget Components",
      y = "Average Expense")
}
distribution_of_spending_south_metro_region <- distribution_of_spending_metro_region("south")

distribution_of_spending_northeast_metro_region <- distribution_of_spending_metro_region("northeast")

distribution_of_spending_midwest_metro_region <- distribution_of_spending_metro_region("midwest")

ggsave("figures/multivariate/question1/question1_figure5.png", distribution_of_spending_south_metro_region)

ggsave("figures/multivariate/question1/question1_figure6.png", distribution_of_spending_northeast_metro_region)

ggsave("figures/multivariate/question1/question1_figure7.png", distribution_of_spending_midwest_metro_region)


# function to look at the nonmetro and metro of a region's different family types average distribution of spending in categories
distribution_of_spending_family_region_metro <- function(region_name) {
  
  fbc_data %>% filter(region == region_name) %>%
  select(ends_with("_annual"), metro,family) %>% 
  summarise(
    avg_housing_annual = mean(housing_annual),
    avg_food_annual = mean(food_annual),
    avg_transportation_annual = mean(transportation_annual),
    avg_healthcare_annual = mean(healthcare_annual),
    avg_other_necessities_annual = mean(other_necessities_annual),
    avg_childcare_annual = mean(childcare_annual),
    avg_taxes_annual = mean(taxes_annual),
    avg_total_annual = mean(total_annual),
    .by = c(metro,family)
  ) %>%
  pivot_longer(cols = -c(metro,family), 
               names_to = "budget_component", values_to = "cost") %>%
  ggplot(aes(budget_component,cost, fill = metro)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~family) +
  theme_light() + 
  scale_x_discrete(labels = c("childcare","food", "healthcare","housing","other necessities","taxes","total","transportation")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(
      title = paste("Distribution of Family Size Spending in the Nonmetro and Metro",
                    str_to_title(region_name)),
      x = "Budget Components",
      y = "Average Expense",
      fill = "Metro Classification") + 
    scale_fill_discrete(labels = c("Nonmetro","Metro"))
}
dist_of_spending_south_fm <- distribution_of_spending_family_region_metro("south")
dist_of_spending_northeast_fm <- distribution_of_spending_family_region_metro("northeast")
dist_of_spending_midwest_fm <- distribution_of_spending_family_region_metro("midwest")

ggsave("figures/multivariate/question1/question1_figure8.png", dist_of_spending_south_fm)

ggsave("figures/multivariate/question1/question1_figure9.png", dist_of_spending_northeast_fm)

ggsave("figures/multivariate/question1/question1_figure10.png", dist_of_spending_midwest_fm)

# consistently healthcare seems to be the only areas where on average families who live in nonmetro areas seem to allocate more on their total expense to it than families in metro areas. 
# I was especially suprised by the closeness in expenses for transportation
# for metro and nonmetro areas. Especially for the metro northeast and metro midwest, as I feel they are known for having the best public transportation in the United States of America, thus thinking that families that live outside of those metro areas would pay a lot more as they have to have a car and pay for those associated expenses. In everyone other category, it is reasonable and makes sense to assume that families living in metro areas, regardless of size, will pay more than that of families in the nonmetro areas. However thinking on it now I find it interesting that the largest gap between transportation for the metro and nonmetro areas is when the families consist of no children, thus bringing light to how having children more often than not means that the parent is going to have to get a car, decreasing that space between the different allocation of transportation expenses.

# function to compare median income to total annual expenses for each state 
# divided at the region level
# have to fix the function it is not working 
median_income_total_annual_state <- function(region_name) {
  
  fbc_data %>% 
    filter(region == region_name) %>% 
    summarise(
      median_family_income = mean(median_family_income),
      avg_total_annual = mean(total_annual),
      .by = c(state_abv, family)
    ) %>%
    ggplot(aes(x = median_family_income, y = avg_total_annual)) +
    geom_jitter(aes(color = state_abv), size = 2) +
    labs(
      title = sprintf("Comparison of Median Income to Cost of Living For %sern States", str_to_title(region_name)),
      x = "Median Family Income",
      y = "Average Cost of Living",
      color = "State"
    ) +
    facet_wrap(~family) + 
    theme_light() + 
    scale_x_continuous(labels = scales::comma) + 
    scale_y_continuous(labels = scales::comma)
} 

median_income_total_state_west <- median_income_total_annual_state("west")
median_income_total_state_south <- median_income_total_annual_state("south")
median_income_total_state_northeast <- median_income_total_annual_state("northeast")
median_income_total_state_midwest <- median_income_total_annual_state("midwest")

ggsave("figures/multivariate/question1/question1_figure11.png", median_income_total_state_west)
ggsave("figures/multivariate/question1/question1_figure12.png", median_income_total_state_south)
ggsave("figures/multivariate/question1/question1_figure13.png", median_income_total_state_northeast)
ggsave("figures/multivariate/question1/question1_figure14.png", median_income_total_state_midwest)

## Next Steps ----
# Consider diving deeper into specific regions or states to understand localized factors influencing the cost of living.
# Explore correlations between income levels, education, and cost of living to identify potential drivers of disparities.
# Explore trends in both monthly and total values. You can observe how each category varies on a monthly basis compared to the overall total. Check if there are specific months where certain categories significantly deviate from their overall totals.









