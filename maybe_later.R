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

glimpse(fs_pin)