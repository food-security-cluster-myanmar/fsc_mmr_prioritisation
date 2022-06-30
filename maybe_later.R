# unused trees
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

tree5 <-  survey %>% 
  rpart(priority ~  shocks_lostwork + shocks_sicknessdeath + shocks_foodprices + shocks_conflict + shocks_cantworkbusiness +
          shocks_naturalhazard + shocks_accessmarket + shocks_pricesother + shocks_other + 
          hoh_female + rural + not_improved_sanitation + children_0_4 + agri_activity + hh_size,
        data = .,  weights = combined_wt, cp = .0091)

fancyRpartPlot(tree5, digits = -3, sub = "", palettes = "Blues", type = 2)

plotcp(tree5)


fancyRpartPlot(tree6, digits = -3, sub = "", palettes = "Blues", type = 2)

plotcp(tree6)


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

# let's not recalculate the vulnerability score 
# i don't think it's really that useful 

conflict_score %>% 
  select(admin3_pcode, battles, explosions_remote_violence, protests_and_riots, 
         strategic_developments, violence_against_civilians) %>% 
  pivot_longer(cols = c(battles:violence_against_civilians), names_to = "event_type", values_to = "value") %>% 
  left_join(fs_pin %>% select(admin3_pcode, state), by = "admin3_pcode") %>% 
  group_by(state) %>% 
  mutate(total_events = sum(value)) %>% 
  ungroup() %>% 
  mutate(state = fct_reorder(state, -total_events)) %>% 
  ungroup() %>% 
  ggplot(aes(x = state, y = value, fill = event_type)) + 
  geom_col() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.7, hjust = 0.7), 
        plot.caption = element_text(hjust = 0.5)) +
  labs(x = "", 
       y = "Conflict events",
       title = "2021 conflict events by state",
       caption = "Data source: ACLED; acleddata.com")

fs_pin %>%  
  mutate(vul_pop = vul_env * population_2021_proj) %>% 
  group_by(state) %>% 
  summarise(vul_pop = sum(vul_pop), 
            total_pop = sum(population_2021_proj)) %>% 
  mutate(vul_pc = vul_pop / total_pop, 
         state = fct_reorder(state, vul_pop)) %>%  
  ggplot(aes(x = state, y = vul_pop, fill = vul_pc)) + 
  geom_col() + 
  scale_y_continuous(labels = comma, breaks = seq(0, 3000000, by = 500000)) + 
  scale_fill_viridis(option = "inferno", direction = -1) + 
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust = .9), 
        plot.caption = element_text(hjust = .5)) + 
  labs(x = "", y = "Estimated vulnerable population", fill = "vul%",
       title = "Vulnerability by state", 
       caption = "Data source: MIMU and ACLED (acleddata.com)")

# alternative clusterings
km_res3 <- fs_pin %>% 
  select(population_density, population_2021_proj, conflict_score, mdp_score) %>%
  mutate_at(vars(population_density:mdp_score), ~ range_wna(.x)) %>% 
  kmeans(8, nstart = 25)


km_res <- fs_pin %>% 
  select(population_density, conflict_score, mdp_score) %>%
  mutate_at(vars(population_density:mdp_score), ~ range_wna(.x)) %>% 
  kmeans(6, nstart = 25)

# the other scatter plot shows the divergence much better 

vul_scatter <- fs_pin %>% 
  mutate(total_events = battles + explosions_remote_violence + protests_and_riots + 
           strategic_developments + violence_against_civilians, 
         vul_env = round(vul_env, digits = 3), 
         fs_pin = round(fs_pin, digits = 1)) %>% 
  ggplot(aes(y = vul_env, x = population_density, 
             text = paste0(township, ",", "\n",
                           state, ",", "\n",
                           "vulnerability_score: ", vul_env, "\n", 
                           "PIN: ", fs_pin, "\n", 
                           "conflict_events: ", total_events, "\n",
                           "fatalities: ", fatalities, "\n",
                           "partners_2021: ", partners_2021))) + 
  geom_hline(yintercept = .5, colour = "red", lty = 2) + 
  geom_vline(xintercept = 300, colour = "red", lty = 2) + 
  geom_text(x = .1, y = .94, label = "C", colour = "grey", size = 7) +
  geom_text(x = 4.4, y = .94, label = "D", colour = "grey", size = 7) +
  geom_text(x = .1, y = .44, label = "E", colour = "grey", size = 7) + 
  geom_text(x = 4.4, y = .44, label = "F", colour = "grey", size = 7) + 
  geom_point(aes(size = fs_pin, colour = conflict_score), alpha = .7) +
  scale_colour_viridis(option = "magma", direction = -1) + 
  # scale_colour_viridis(option = "turbo", direction = -1, trans = "log10") +
  scale_x_continuous(trans = "log10", breaks = c(0, 1, 10, 30, 100, 300, 1000, 3000, 10000), 
                     labels = comma_format(accuracy = 1)) + 
  labs(x = "Persons per km2", y = "Vulnerability score", size = "PIN",
       colour = "conflict\nscore",
       title = "Vulnenrability and population density by township")

ggplotly(vul_scatter, tooltip = c("text"), width = 820) %>%  
  config(displayModeBar = FALSE) %>% 
  layout(title = list(text = paste0("Vulnerability and population density by township",
                                    "<br>", 
                                    "<sup>", 
                                    "Vulnerability score marked by colour; FS PIN marked by size", "</sup>")), 
         colorscale = list(font = list(size = 4)))

# i think that conflict per capita is not necessarily the most useful indicator,
# since the amount of conflict is still important in determining how much an area has been affected 
# i.e. if there have been many battles in one area with a large population, they're probably worse off 
# than a small area with only one battle 
# i think that's the asssumption i'll go with  

fs_pin %>% 
  mutate(conflict_pc = conflict_score / population_2021_proj, 
         conflict_pc = ifelse(is.infinite(conflict_pc), 0, conflict_pc)) %>% 
  ggplot(aes(x = mdp_score, y = conflict_pc)) + 
  geom_point(aes(size = population_2021_proj), alpha = .5) +
  scale_y_continuous(trans = "log10") + 
  geom_smooth(method = "lm")

# the new version is just much better
scatter_groups <- fs_pin %>% 
  mutate(total_events = battles + explosions_remote_violence + protests_and_riots + strategic_developments +
           violence_against_civilians) %>% 
  ggplot(aes(x = total_events, y = fatalities, colour = cluster, 
             text = paste0(township, ",", "\n",
                           state, ",", "\n",
                           "group: ", cluster, ",", "\n",
                           "conflict_score: ", conflict_score, ",", "\n",
                           "fatalities: ", fatalities, ",", "\n",
                           "vul_score: ", mdp_score))) +
  geom_point(aes(size = fatalities), alpha = 0.8) +
  scale_y_continuous(trans = "log10", breaks = c(0, 1, 10, 30, 100, 300, 1000)) +
  scale_x_continuous(trans = "log10", breaks = c(0, 1, 10, 30, 100, 300)) +
  scale_colour_viridis_d(option = "cividis") + 
  # scale_colour_manual(values = c("#575C6DFF", "#00204DFF", "#C4B56CFF", "#FFEA46FF")) +
  geom_hline(aes(yintercept = mean(fatalities, na.rm = TRUE)), colour = "red", lty = 2) +
  geom_vline(aes(xintercept = mean(total_events, na.rm = TRUE)), colour = "red", lty = 2) +
  labs(x = "Conflict events", 
       y = "Fatalities",
       title = "Conflict events and fatalities by prioritisation group", 
       subtitle = "Means of both axes are marked by the dotted red line",
       colour = "group", 
       size = "fatalities",
       caption = "Data source: ACLED; acleddata.com") 



ggplotly(scatter_groups, tooltip = c("text"), width = 820) %>% 
  # layout(showlegend = FALSE, legend = list(font = list(size = 6))) %>% 
  config(displayModeBar = FALSE) %>% 
  layout(title = list(text = paste0("Conflict events and fatalities by prioritisation group",
                                    "<br>",
                                    "<sup>",
                                    "mouse over for details; means marked by red lines; fatalities marked by size","</sup>")))

# no longer needed, also makes things unclear
pop_scatter <- fs_pin %>% 
  mutate(total_events = battles + explosions_remote_violence + protests_and_riots + strategic_developments +
           violence_against_civilians) %>% 
  ggplot(aes(x = fs_pin, y = population_density, colour = cluster,
             text = paste0(township, ",", "\n",
                           state, ",", "\n",
                           "group: ", cluster, ",", "\n",
                           "events: ", total_events, ",", "\n",
                           "fatalities: ", fatalities, ",", "\n",
                           "partners_2021: ", partners_2021))) +
  geom_point(aes(size = conflict_score), alpha = 0.8) +
  # scale_colour_manual(values = c("#575C6DFF", "#00204DFF", "#C4B56CFF", "#FFEA46FF")) +
  scale_colour_viridis_d(option = "cividis") +
  scale_x_continuous(labels = comma, trans = "log10") +
  scale_y_continuous(labels = comma_format(accuracy = 1), trans = "log10") +
  geom_hline(aes(yintercept = mean(population_density, na.rm = TRUE)), colour = "red", lty = 2) +
  geom_vline(aes(xintercept = mean(fs_pin, na.rm = TRUE)), colour = "red", lty = 2) +
  labs(x = "People in need", 
       y = "Population density",
       title = "People in need and population density by prioritisation group", 
       subtitle = "Means of both axes are marked by the dotted red line",
       colour = "group", 
       size = "conflict\nscore",
       caption = "Data source: ACLED; acleddata.com") 

ggplotly(pop_scatter, tooltip = c("text"), width = 820) %>% 
  # layout(showlegend = FALSE, legend = list(font = list(size = 6))) %>% 
  config(displayModeBar = FALSE) %>% 
  layout(title = list(text = paste0("People in need and population density by prioritisation group",
                                    "<br>",
                                    "<sup>",
                                    "mouse over for details; means marked by red lines; conflict score marked by size","</sup>")))

# old summary stat by group table
fs_pin %>% 
  group_by(group) %>% 
  summarise(PIN = sum(fs_pin), 
            target = sum(fs_targeted),
            conflict_score = mean(conflict_score),
            ppl_per_km2 = mean(population_density),
            townships = n()) %>% 
  mutate_at(vars(c(conflict_score)), ~round(., digits = 3)) %>% 
  mutate_at(vars(c(PIN, ppl_per_km2, target)), ~ round(.)) %>% 
  kable(caption = "Summary statistics by township group", format.args = list(big.mark = ",")) %>% 
  kable_classic_2("striped") %>% 
  footnote(footnote_as_chunk = TRUE, threeparttable = TRUE, 
           general = "Groups A1 and A2 have above average conflict scores; groups A1 and B1 have the above average population densities. Townships in groups A1 and A2 should be prioritised over the others.") 

# %>%
#  save_kable(file = "summary_stats_groups.png", zoom = 2)

# unused text from old prioritisation groups
# It is additionally recognised that different types of townships necessitate different programming options. And, as established in the first section, rural and urban areas have widely differing numbers of households in the priority group. Therefore, townships have also been split into four simple groups (A1, A2, B1 and B2) based on their conflict score and population density. 

# This grouping separates all 330 townships along two criteria -- **high (A) or low (B) conflict score and high (1) or low (2) population density**. Groups A1 and A2 have above average conflict scores. These 107 townships should be prioritised for humanitarian interventions. Recalling the scatterplot above, the colours have now been updated to reflect the prioritisation group. From the plot below that whilst group A1 have populations that are easier to access (having higher population density), the incidence of conflict is higher overall in group A2, with the numbers of conflict-related fatalities being much higher. This quick-and-dirty prioritisation has managed to exclude almost all the townships in bottom-left quadrant (least conflict-affected) from groups A1 and A2. 

# Groups A1 and A2 can be distinguished by their population density, with the average population density in group A1 being more than 100 times higher than in group A2. The average PIN per township is slightly higher in group A1 than in group A2. The scatterplot below shows townships by the number of people in need (x-axis) and the population density (y-axis), with the colours reflecting which group each belongs to:


fsc %>% 
  filter(township == "Minbya") %>% 
  summarise(beneficiaries = sum(new_beneficiaries))

fs_pin %>% 
  mutate(vul_ranking = dense_rank(desc(mdp_adjust))) %>% 
  select(state, admin1_pcode, township, admin3_pcode,
         total_population_2021proj = population_2021_proj, 
         conflict_ranking, flood_ranking, vul_ranking, 
         food_security_pin_2022 = fs_pin, food_security_target_2022 = fs_targeted, 
         beneficiaries_2022 = beneficiaries) %>% 
  left_join(fsc %>%
              mutate(activity_red = case_when(activity %in% c("food distributions (in kind/voucher/cash), moderate", 
                                                              "food distributions (in kind/voucher/cash), severe") ~ 
                                                "food distribution",
                                              activity %in% c("multi-purpose cash transfer (MPC), moderate",
                                                              "multi-purpose cash transfer (MPC), severe") ~ 
                                                "multi-purpose cash transfer",
                                              activity == "livelihoods vocational training" ~ "vocational training",
                                              activity == "food/cash for work/assets" ~ "food_cash for work_assets",
                                              activity == "income-generating activities and small grants" ~ "IGA and small grants", 
                                              TRUE ~ activity), 
                     activity_red = str_remove_all(activity_red, "provision of ")) %>% 
              group_by(admin3_pcode, activity_red) %>% 
              summarise(beneficiaries = sum(reached_beneficiaries)) %>% 
              pivot_wider(names_from = activity_red, values_from = beneficiaries, values_fill = 0), 
            by = "admin3_pcode") %>% 
  write_csv("township_indicators_small.csv")

