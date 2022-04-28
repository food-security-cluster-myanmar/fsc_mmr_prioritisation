tree1 <- fs_pin %>% 
  # mutate_at(vars(c(conflict_score, disaster_prob, population_density)), scale) %>% 
  rpart(conflict_score ~ population_density + fs_pin, 
        data = ., minbucket = 50)

fancyRpartPlot(tree1, digits = -3, sub = "", palettes = "Blues", type = 2)

# this tree is really not useful 
fs_pin <- fs_pin %>% 
  mutate(rule1 = row.names(tree1$frame)[tree1$where]) %>%
  left_join(rpart.rules.table(tree1) %>% 
              filter(Leaf == TRUE) %>% 
              rename(rule1 = Rule) %>% 
              group_by(rule1) %>% 
              summarise(subrules1 = paste(Subrule, collapse = ",")),
            by = c("rule1", "subrules1"))


# large patchwork plot
fs_pin %>%
  ggplot(aes(x = fs_pin, y = conflict_score, colour = population_density)) + 
  geom_hline(aes(yintercept = median(conflict_score)), colour = "red", lty = 2) +
  geom_vline(aes(xintercept = median(fs_pin)), colour = "red", lty = 2) +
  geom_point(alpha = 0.8) +
  scale_x_continuous(labels = comma, trans = "log10") +    
  scale_colour_viridis(trans = "log10", direction = -1) +
  labs(y = "Conflict score", 
       title = "Townships by FS PIN and conflict score") + 
  
  
  fs_pin %>% 
  ggplot(aes(x = fs_pin, y = pin_pop, colour = population_density)) +
  geom_hline(aes(yintercept = median(pin_pop)), colour = "red", lty = 2) +
  geom_vline(aes(xintercept = median(fs_pin)), colour = "red", lty = 2) +
  geom_jitter(alpha = 0.8) +
  scale_x_continuous(labels = comma, trans = "log10") +
  scale_colour_viridis(trans = "log10", direction = -1) +
  labs(y = "PIN as % of township pop.", 
       title = "Townships by FS PIN and % of township pop. in PIN") +
  
  fs_pin %>%
  ggplot(aes(x = fs_pin, y = conflict_score, colour = rule1)) + 
  geom_hline(aes(yintercept = median(conflict_score)), colour = "red", lty = 2) +
  geom_vline(aes(xintercept = median(fs_pin)), colour = "red", lty = 2) +
  geom_point(alpha = 0.8) +
  scale_x_continuous(labels = comma, trans = "log10") +
  labs(y = "Conflict score", 
       title = "Townships by FS PIN and conflict score") + 
  
  
  fs_pin %>% 
  ggplot(aes(x = fs_pin, y = pin_pop, colour = rule1)) +
  geom_hline(aes(yintercept = median(pin_pop)), colour = "red", lty = 2) +
  geom_vline(aes(xintercept = median(fs_pin)), colour = "red", lty = 2) +
  geom_jitter(alpha = 0.8) +
  scale_x_continuous(labels = comma, trans = "log10") +
  labs(y = "PIN as % of township pop.", 
       title = "Townships by FS PIN and % of township pop. in PIN") 

# i like the new scatter plots much better 

fs_pin %>%
  ggplot(aes(x = fs_pin, y = conflict_score, colour = group)) + 
  geom_hline(aes(yintercept = mean(conflict_score)), colour = "red", lty = 2) +
  geom_vline(aes(xintercept = median(fs_pin)), colour = "red", lty = 2) +
  geom_point() +
  scale_x_continuous(labels = comma, trans = "log10") +    
  # scale_colour_manual(values = c("#FD9567FF", "#CD4071FF", "#231151FF", "#000004FF")) +
  scale_colour_viridis_d(end = 0.9, option = "cividis") +
  labs(y = "Conflict score", 
       title = "Townships by FS PIN and conflict score",
       x = "People in need",
       subtitle = "Average of each axis marked by dotted red line") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1), order = 1)) +
  
  
  fs_pin %>% 
  ggplot(aes(x = fs_pin, y = population_density, colour = group)) +
  geom_hline(aes(yintercept = mean(population_density)), colour = "red", lty = 2) +
  geom_vline(aes(xintercept = median(fs_pin)), colour = "red", lty = 2) +
  geom_point() +
  scale_x_continuous(labels = comma, trans = "log10") +
  scale_y_continuous(labels = comma_format(accuracy = 1), trans = "log10") +
  # scale_colour_manual(values = c("#FD9567FF", "#CD4071FF", "#231151FF", "#000004FF")) +
  scale_colour_viridis_d(end = 0.9, option = "cividis") +
  labs(y = "Persons per sq km", 
       title = "Townships by FS PIN and population density", 
       x = "People in need",
       subtitle = "Average of each axis marked by dotted red line") +
  guides(colour = guide_legend(override.aes = list(alpha = 1), order = 1))

ggsave("tsp_groups.png", dpi = 300, height = 6, width = 15, units = "in")

# writing the fsc pin csv  

fs_pin %>% 
  select(state, admin1_pcode, township, admin3_pcode,
         population_2021_proj, population_density, 
         fs_pin, fs_targeted, group, 
         conflict_score, battles, explosions_remote_violence, protests_and_riots, 
         strategic_developments, violence_against_civilians, fatalities,
         flood_count, flood_prob, 
         beneficiaries_2021, partners_2021, not_in_6months) %>% 
  write_csv("ref_data.csv")

# 39 townships csv 

fs_pin %>% 
  mutate(mean_pop = mean(population_2021_proj)) %>% 
  filter(conflict_score >= quantile(conflict_score, 0.75)) %>% 
  filter(not_in_6months == 1) %>% 
  filter(population_2021_proj >= mean_pop) %>% 
  arrange(desc(conflict_score)) %>% 
  select(state, admin1_pcode, district, township, admin3_pcode, 
         population_2021_proj, population_density,
         fs_pin, fs_targeted, conflict_score) %>% 
  write_csv("39_townships.csv")

# failed highcharter -- now using plotly
group_colours <-c("#575C6DFF", "#00204DFF", "#C4B56CFF", "#FFEA46FF")

x <- c("Events", "Fatalities", "Group")
y <- sprintf("{point.%s:.2f}", c("total_events", "fatalities", "group"))

tltip <- tooltip_table(x, y)

fs_pin %>% 
  mutate(total_events = battles + explosions_remote_violence + protests_and_riots + strategic_developments +
           violence_against_civilians,
         p_group = factor(group)) %>% 
  hchart("scatter",
         hcaes(x = total_events, y = fatalities, color = p_group, size = fatalities), 
         minSize = 2, maxSize = 20) %>% 
  hc_xAxis(title = list(text = "Conflict events"), gridLinewidth = 0, type = "logarithmic",
           plotLines = list(list(value = mean(fs_pin$fatalities, na.rm = TRUE), 
                                 width = 3, color = "red"))) %>% 
  hc_yAxis(title = list(text = "Fatalities"), gridLinewidth = 0, type = "logarithmic",
           plotLines = list(list(value = mean(fs_pin$total_events, na.rm = TRUE), 
                                 width = 3, color = "red"))) %>% 
  hc_title(text = "Conflict events and fatalities by prioritisation group") %>% 
  hc_subtitle(text = "Means of both axes are marked by the dotted red lines") %>% 
  hc_tooltip(useHTML = TRUE, headerformat = "", pointFormat = tltip) %>% 
  hc_size(height = 700)

survey %>% 
  mutate(food_expense_mmk = income_main_amount * expense_food) %>%
  ggplot(aes(x = hhfcs_inv, y = food_expense_mmk)) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  scale_y_continuous(trans = "log10") + 
  labs(x = "Food consumption score (inverse)",
       y = "Household food expenditures (MMK)", 
       title = "Relationship between ")

survey %>% 
  count(state) %>% 
  mutate(state = fct_reorder(state, n)) %>% 
  ggplot(aes(x = n, y = state, fill = state)) + 
  geom_col() +
  theme(legend.position = "none") + 
  labs(x = "Number of households", 
       y = "",
       title = "Households interviewed by state, WFP-FAO survey")

survey %>% 
  filter(!is.na(food_expenses_mmk)) %>% 
  select(contains("shocks_"), food_expenses_mmk, fies_raw_range, hhfcs_inv, csi_weighted, fs_score) %>% 
  select(-shocks_none) %>% 
  cor(method = c("pearson")) %>% 
  corrplot(type = "upper", col = brewer.pal(n = 8, name = "RdYlBu"),
           addCoef.col = 1, number.cex = 0.5,
           tl.srt = 35, tl.cex = 0.75,
           title = "Correlations between shocks and food security indicators \n",  mar=c(0,0,2,0), diag = FALSE)


survey_long %>%  
  filter(str_detect(var, "shocks_|hoh_|rural|not_improved|agri_activity|income_ms_|edu_") |
           var %in% c("priority")) %>% 
  lm_prep_long() %>% 
  pivot_longer(cols = -priority, names_to = "var", values_to = "value") %>% 
  group_by(var, priority) %>% 
  summarise(mean = mean(value)) %>% 
  mutate(priority = recode(priority, `0` = "not_priority", `1` = "priority")) %>% 
  ggplot(aes(x = mean, y = reorder(var, mean), fill = mean)) + 
  geom_col() + 
  scale_fill_viridis(option = "plasma") +
  geom_text(aes(label = scales::percent(round(mean, digits = 2))), size = 3, hjust = -0.1) + 
  labs(x = "Mean", y = "", 
       title = "Prevalence of environmental and demographic indicators") + 
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 8, face = "bold"), 
        plot.title = element_text(size = 10)) +
  expand_limits(x = 1.05) +
  facet_wrap(~priority) + 
  scale_y_reordered()

survey_long %>%  
  filter(str_detect(var, "shocks_|hoh_|rural|expense_food|not_improved|agri_activity|income_ms_|edu_") |
           var %in% c("priority", "expense_food")) %>% 
  lm_prep_long() %>% 
  lm(priority ~ ., data = ., family = quasibinomial) %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(term = str_replace(term, "income_ms", "income_source"),
         term = str_replace(term, "hoh_age", ""),
         term = str_replace(term, "hoh_education", ""), 
         term = fct_reorder(term, estimate)) %>% 
  ggplot(aes(x = estimate, y = term, colour = estimate)) + 
  geom_vline(xintercept = 0, lty = 2, colour = "red") +
  geom_point() + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  scale_colour_viridis(option = "turbo") +
  labs(x = "Estimate", y = "", 
       title = "Predictive performance of environmental and demographic variables") +
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 8.5),
        axis.title.x = element_text(size = 8, face = "bold"), 
        plot.title = element_text(size = 11))

conflict_score %>% 
  left_join(pcodes %>% 
              select(admin1_pcode = SR_Pcode, admin3_pcode = Tsp_Pcode), by = "admin3_pcode") %>% 
  group_by(admin1, admin1_pcode) %>% 
  summarise(mean_score = mean(score_i), 
            mean_fatalities = mean(fatalities)) %>%
  left_join(survey %>% 
              group_by(admin1_pcode) %>%  
              summarise(respondents = n()), by = "admin1_pcode") %>% 
  ungroup() %>% 
  mutate(in_survey = !is.na(respondents),
         in_survey = ifelse(respondents < 10 & !is.na(respondents), FALSE, in_survey),
         admin1 = fct_reorder(admin1, mean_score)) %>%
  ggplot(aes(x = mean_score, y = admin1, fill = in_survey)) + 
  geom_col() +
  labs(x = "Average conflict score", y = "")

survey %>% 
  filter(income_ms_stable_non_ag == 1 & rural == 0) %>%
  conf_impact() %>% 
  mutate(category = "urban_workers") %>% 
  rbind(
    survey %>% 
      filter(income_ms_agriculture == 1 & rural == 1) %>% 
      conf_impact() %>% 
      mutate(category = "rural_farmers")
  ) %>% 
  pivot_wider(names_from = shocks_conflict, values_from = mean_score, names_prefix = "conflict_") %>% 
  mutate(diff = round((conflict_yes - conflict_no) / conflict_no * 100, digits = 2)) %>% 
  arrange(desc(diff)) %>% 
  select(category, hoh_education, `%_difference` = diff) %>%
  kable(caption = "Percentage difference in food security scores") %>% 
  kable_classic_2("striped", full_width = FALSE, position = "left")