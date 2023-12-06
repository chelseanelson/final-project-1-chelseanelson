## Question of Interest ----
# How does the cost of living vary across different geographical regions?

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
  .by = c(region, metro)) %>% ggplot(aes(region, avg_cost_of_living)) + 
  geom_col(aes(fill = metro), position = "dodge") + labs(
    title = "Average Cost of Living By Metro Classification and Region",
    x = "Region",
    y = "Average Cost of Living",
    fill = "Metro\nClassification"
  ) + theme_light() + 
  scale_fill_discrete(labels = c("Non-Metro Area", "Metro Area")) + 
  scale_x_discrete(labels = c("South", "West", "Northeast","Midwest"))

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

# from this plot we see that it would be interesting to look further into the # metro south, non-metro northeast, metro midwest, metro and non-metro west

# again we see the same relationship as before, but now interestingly 
# we see that the people in the west have extremely bad ratio between
# the amount of money they make and the actual average cost of living in 
# the west. 

# why is this? 
# housing market, regional economic disparities (certain areas may be more affluent than others), cost of healthcare, income inequalities


# on the flip side we see that people in the midwest and metro northeast make more money than they spend on average, being the only groups in this position
# as everyone else makes less money than they have to spend on average. Is
# this related to having the best access to public transport,  
