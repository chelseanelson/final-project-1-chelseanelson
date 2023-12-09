## Question of Interest ----
# 2. Are there discernible patterns or trends in budget allocation that stand out, considering family types and monthly expenses? 
  
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
  geom_text(aes(label = sprintf("$%.2f", avg_total_annual)), size = 2.5, 
            position = position_dodge(width = 0.9), vjust = -0.25) + 
  geom_col() + 
  facet_wrap(~family) + 
  labs(title = "Average Total Annual Expenses by Region and Family Type",
       x = "Region",
       y = "Total Annual Cost",
       fill = "Family Type") +
  theme_light() +
  scale_x_discrete(labels = c("South", "West", "Northeast", "Midwest")) + 
  scale_y_continuous(labels = scales::comma)

ggsave("figures/multivariate/question2/question2_figure1.png",total_annual_expenses_family_region)


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
  summarise(average_percentage  = mean(percentage),
            .by = c(budget_component, family)) %>%
  ggplot(aes(x = budget_component, y = family, fill = average_percentage)) +
  geom_tile() +
  geom_text(aes(label = scales::percent(average_percentage, accuracy = 1))) +
  labs(
    title = "Budget Allocation Heatmap",
    x = "Budget Component",
    y = "Family Type",
    fill = "Percentage of Total Annual Cost"
  ) +
  theme_light() +
  scale_fill_viridis_c() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_discrete(labels = c("childcare annual","food annual","healthcare annual","housing annual","other necessities annual", "taxes annual","transportation annual"))

ggsave("figures/multivariate/question2/question2_figure2.png",budget_heatmap)


budget_heatmap_regional_metro <- function(region_name, metro_status) {
  
  if (metro_status == "0") {
    
    fbc_data %>%
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
    filter(region == region_name & metro == metro_status) %>% 
    summarise(average_percentage  = mean(percentage),
              .by = c(budget_component, family)) %>%
    ggplot(aes(x = budget_component, y = family, fill = average_percentage)) +
    geom_tile() +
      geom_text(aes(label = scales::percent(average_percentage, accuracy = 1))) +
    labs(
      title = sprintf("Budget Allocation Heatmap For Nonmetro Areas in %sern States", str_to_title(region_name)),
      x = "Budget Component",
      y = "Family Type",
      fill = "Percentage of Total Annual Cost"
    ) +
    theme_light() +
    scale_fill_viridis_c() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_x_discrete(labels = c("childcare annual","food annual","healthcare annual","housing annual","other necessities annual", "taxes annual","transportation annual"))
  }
    else if (metro_status == "1") {
      
      fbc_data %>%
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
        filter(region == region_name & metro == metro_status) %>% 
        summarise(average_percentage  = mean(percentage),
                  .by = c(budget_component, family)) %>%
        ggplot(aes(x = budget_component, y = family, fill = average_percentage)) +
        geom_tile() +
        geom_text(aes(label = scales::percent(average_percentage, accuracy = 1))) +
        labs(
          title = sprintf("Budget Allocation Heatmap For Metro Areas in %sern States", str_to_title(region_name)),
          x = "Budget Component",
          y = "Family Type",
          fill = "Percentage of Total Annual Cost"
        ) +
        theme_light() + 
        scale_fill_viridis_c() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + scale_x_discrete(labels = c("childcare annual","food annual","healthcare annual","housing annual","other necessities annual", "taxes annual","transportation annual"))
  }
}
budget_heatmap_regional_metro_w0 <- budget_heatmap_regional_metro("west", "0")
budget_heatmap_regional_metro_w1 <- budget_heatmap_regional_metro("west", "1")
budget_heatmap_combined_west <- budget_heatmap_regional_metro_w0 + budget_heatmap_regional_metro_w1

budget_heatmap_regional_metro_s0 <- budget_heatmap_regional_metro("south", "0")
budget_heatmap_regional_metro_s1 <- budget_heatmap_regional_metro("south", "1")
budget_heatmap_combined_south <- budget_heatmap_regional_metro_s0 + buget_heatmap_regional_metro_s1

budget_heatmap_combined_northeast <- 
budget_heatmap_regional_metro_n0 <- budget_heatmap_regional_metro("northeast", "0")
budget_heatmap_regional_metro_n1 <- budget_heatmap_regional_metro("northeast", "1")
budget_heatmap_combined_northeast <- budget_heatmap_regional_metro_n0 + buget_heatmap_regional_metro_n1

budget_heatmap_regional_metro_mw0 <- budget_heatmap_regional_metro("midwest", "0")
budget_heatmap_regional_metro_mw1 <- budget_heatmap_regional_metro("midwest", "1")
budget_heatmap_combined_midwest <- buget_heatmap_regional_metro_mw0 + buget_heatmap_regional_metro_mw1

ggsave("figures/multivariate/question2/question2_figure3.png", budget_heatmap_regional_metro_w0)
ggsave("figures/multivariate/question2/question2_figure4.png", budget_heatmap_regional_metro_w1)
ggsave("figures/multivariate/question2/question2_figure5.png", budget_heatmap_regional_metro_s0)
ggsave("figures/multivariate/question2/question2_figure6.png", budget_heatmap_regional_metro_s1)
ggsave("figures/multivariate/question2/question2_figure7.png", budget_heatmap_regional_metro_n0)
ggsave("figures/multivariate/question2/question2_figure8.png", budget_heatmap_regional_metro_n1)
ggsave("figures/multivariate/question2/question2_figure9.png", budget_heatmap_regional_metro_mw0)
ggsave("figures/multivariate/question2/question2_figure10.png", budget_heatmap_regional_metro_mw1)
ggsave("figures/multivariate/question2/question2_combinedplot1_west.png", budget_heatmap_combined_west)
ggsave("figures/multivariate/question2/question2_combinedplot2_south.png", budget_heatmap_combined_south)
ggsave("figures/multivariate/question2/question2_combinedplot3_northeast.png", budget_heatmap_combined_northeast)
ggsave("figures/multivariate/question2/question2_combinedplot4_midwest.png", budget_heatmap_combined_midwest)

# scatter plot that looks at how median family income compares to minimum wage for each region
minimum_wage_median_income <- ggplot(fbc_data, aes(minimum_wage,  median_family_income)) +
  geom_jitter() + facet_wrap(~region, labeller = labeller(region = c("south" = "South", "west" = "West", "northeast" = "Northeast", "midwest" = "Midwest"))) + 
  labs(title = "Median Family Income vs. Minimum Wage Based on Region",
       x = "Minimum Wage",
       y = "Median Family Income") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) + theme_light()
  
ggsave("figures/multivariate/question2/question2_figure11.png", minimum_wage_median_income)

minimum_wage_cost_of_living <- ggplot(fbc_data, aes(minimum_wage, total_annual, color = family)) +
  geom_jitter() + facet_wrap(~region, labeller = labeller(region = c("south" = "South", "west" = "West", "northeast" = "Northeast", "midwest" = "Midwest"))) + 
  labs(title = "Median Family Income vs. Minimum Wage Based on Region",
       x = "Minimum Wage",
       y = "Total Annual Expenses",
       color = "Family Types") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) + theme_light()


ggsave("figures/multivariate/question2/question2_figure12.png", minimum_wage_cost_of_living)


