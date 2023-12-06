# Correlations and Relationships

# I want to look at how median family income correlates to total annual costs 
# on the state, regional, and metro levels

## case studies 
# most expensive state vs. least expensive state 
# most educated vs. least educated state 


# How does the cost of living vary across different geographical regions?

avg_cost_of_living_region_metro <- fbc_data %>% summarise(
  avg_cost_of_living = mean(total_annual),
  .by = c(region, metro)) %>% ggplot(aes(region, avg_cost_of_living)) + 
  geom_col(aes(fill = metro), position = "dodge") + labs(
   title = "Average Cost of Living By Metro Classification and Region",
   x = "Region",
   y = "Average Cost of Living",
   fill = "Metro\nClassification"
  ) + theme_light() + 
  scale_fill_discrete(labels = c("Non-Metro Area", "Metro Area")) + 
  scale_x_discrete(labels = c("South", "West", "Northeast","Midwest"))

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

# from this plot we see that it would be interesting to look further into the 
# metro south, non-metro northeast, metro midwest, metro and non-metro west 

# follow up questions 
# Are there notable disparities in housing, transportation, or healthcare costs among regions?
# Do families in metro areas face different challenges in meeting their budgets compared to those in non-metro areas?
# Analyze how the metro classification interacts with family types, minimum wage, and the total cost rank.
# Explore how income disparities within regions and metro classifications correlate with the average cost of living.



# Are there patterns or trends in budget allocation that stand out?

total_annual_expenses_family_region <- fbc_data %>%
  group_by(region, family) %>% summarise(
    avg_total_annual = mean(total_annual)
  ) %>% 
  ggplot(aes(x = region, y = avg_total_annual)) +
  geom_col() + 
  facet_wrap(~family) + 
  labs(title = "Average Total Annual Expenses by Region and Family Type",
       x = "Region",
       y = "Total Annual Cost",
       fill = "Family Type") +
  theme_light() +
  scale_x_discrete(labels = c("South", "West", "Northeast", "Midwest")) + 
  scale_y_continuous(labels = scales::comma)

# I want to add numbers to this, right now numbers are overlaying too much
budget_heatmap <- fbc_data %>%
  select(family, region, metro, ends_with("_annual")) %>%
  summarise(
    avg_housing_annual = mean(housing_annual),
    avg_food_annual = mean(food_annual),
    avg_transportation_annual = mean(transportation_annual),
    avg_healthcare_annual = mean(healthcare_annual),
    avg_other_necessities_annual = mean(other_necessities_annual),
    avg_childcare_annual = mean(childcare_annual),
    avg_taxes_annual = mean(taxes_annual),
    avg_total_annual = mean(total_annual),
    .by = c(region,metro,family)
  ) %>%
  pivot_longer(cols = -c("region","metro","family","avg_total_annual"), 
               names_to = "budget_component", values_to = "cost") %>% 
  relocate(avg_total_annual, .after = cost) %>%
  mutate(
    percentage = cost / avg_total_annual
  ) %>%
  ggplot(aes(x = budget_component, y = family, fill = percentage)) +
  geom_tile() +
  geom_text(aes(label = scales::percent(percentage))) +
  labs(
    title = "Budget Allocation Heatmap",
    x = "Budget Component",
    y = "Family Type",
    fill = "Percentage of Total Annual Cost"
  ) +
  theme_light() +
  scale_fill_viridis_c()

# want to split this up to be more representative of the different regions and 
# metro statuses as well 

# follow-up questions 
# Analyze how different family types allocate their budget across various categories.
# Are there significant variations in budget allocation patterns among family types?
  

# Compare the affordability of living in metro and non-metro areas within states.
# What percentage of the family budget is allocated to different categories (housing, food, healthcare, etc.) across states?

# working on something similar to this right now going to make a function that looks at
# this for any state, and then preferably compare popular or most influence states in terms of cost of living and such 


# questions to assess 
# Explore how regional factors, minimum wage, and median family income collectively influence the family budget. 
# Analyze how the metro classification interacts with family types, minimum wage, and the total cost rank.
# Are there notable differences in family budget challenges between metro and non-metro areas within states?
# How does the family budget compare to the median income in each state? 


fbc_data %>% summarise(
  median_family_income = mean(median_family_income),
  avg_total_annual = mean(total_annual),
  .by = c(state_abv, family)
) %>%
ggplot(aes(x = median_family_income, y = avg_total_annual)) +
  geom_point(aes(color = state_abv)) +
  labs(
    title = "Comparison of Family Budget to Median Income",
    x = "Median Family Income",
    y = "Total Annual Family Budget",
    color = "State"
  ) +
  facet_wrap(~family) + 
  theme_light() + scale_x_continuous(labels = scales::comma)


fbc_data %>% 
  filter(state_abv == "TX" & family == "2p2c") %>%
  ggplot(aes(median_family_income, total_annual)) + 
  geom_point(aes(shape = metro))
  