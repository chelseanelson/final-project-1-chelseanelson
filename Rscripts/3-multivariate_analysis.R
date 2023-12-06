# Correlations and Relationships

# I want to look at how median family income correlates to total annual costs 
# on the state, regional, and metro levels

## case studies 
# most expensive state vs. least expensive state 
# most educated vs. least educated state 

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
  