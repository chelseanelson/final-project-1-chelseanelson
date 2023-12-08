## Question of Interest ----
# 2. Are there discernible patterns or trends in budget allocation that stand out, considering family types and monthly expenses? 
  
#Building on the foundation of my previous analyses, I now shift focus more heavily on the allocation of expenses. Exploring patterns and trends in budget allocation, I aim to identify distinctive markers that stand out amidst the diverse financial landscape pertaining to the different family types and the relationship between monthly and annual expenses. Thus, looking to see how family size impacts budget allocation. And if there are specific categories where larger families allocate a significantly higher percentage of their budget.

# For me: I will work on the heatmap and see how that looks. Again look from the national, regional, and state levels. And then also I want to pick two states one being the best in a category and one being the worst and then look at how the counties compare for those two states as well. Follow-up question: What is the impact of the number of working adults in a family on budget allocation? Do dual-income families allocate their budgets differently compared to single-income families?

### Follow-Up Questions -----
# Analyze how different family types allocate their budget across various categories.
# Are there significant variations in budget allocation patterns among family type?
# What percentage of the family budget is allocated to different categories (housing, food, healthcare, etc.) across states?


### Start of Analysis ----
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