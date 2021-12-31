passing_pbp$penalty[is.na(passing_pbp$penalty)] <- 'None'

epa_data4 <- epa_data %>% 
  mutate(score_differential = off_score - def_score) %>% 
  select(game_id, play_id, yards_to_go, score_differential)

passing_pbp2 <- passing_pbp %>% 
  left_join(epa_data4, by = c('game_id', 'play_id'))
  

passing_pbp2 <- passing_pbp2 %>% 
  mutate(
    DPI = ifelse(penalty == "Defensive pass interference", 1, 0)
  ) %>% 
  filter(!is.na(yards_to_go))

passing_pbp2 %>% 
  filter(penalty == "Defensive pass interference") %>% 
  summarise(
    EPA = mean(EPA)
  )

DPI_model <- passing_pbp2 %>% 
  select(down, distance, DPI, yards_to_go, score_differential) %>% 
  mutate(
    down = as.factor(down)
  ) %>% 
  filter(!is.na(yards_to_go))

str(DPI_model)
colSums(is.na(DPI_model))

DPI_lm <- lm(DPI ~ down + distance + score_differential + yards_to_go,
             data = DPI_model)

summary(DPI_lm)

vip(DPI_lm, num_features = 7) 


DPI_preds <- data.frame(predict.lm(DPI_lm, newdata = DPI_model)) %>%
  rename(exp_DPI = predict.lm.DPI_lm..newdata...DPI_model.)

DPI_projs <- cbind(passing_pbp2, DPI_preds)


DPI_21 <- DPI_projs %>%
  mutate(DPI_oe = DPI - exp_DPI) %>%
  mutate(offense = case_when(
    offense == "SD" ~ "LAC",
    offense == "BLT" ~ "BAL",
    offense == "OAK" ~ "LV",
    offense == "HST" ~ "HOU",
    offense == "SL" ~ "LA",
    offense == "CLV" ~ "CLE", 
    offense == "ARZ" ~ "ARI",
    TRUE ~ offense
  )) %>% 
  group_by(passer_name, offense) %>%  
  summarize(DPI_plays = sum(DPI),
            avg_DPI_oe = mean(DPI_oe)*100,
            EPA = mean(EPA),
            air_yards = sum(pass_depth[penalty == "Defensive pass interference"]),
            Mean_air_yards = mean(pass_depth[penalty == "Defensive pass interference"])
            ) %>% 
  filter(DPI_plays > 30) %>% 
  left_join(teams_colors_logos, by = c('offense' = 'team_abbr'))

DPI_projs %>% 
  filter(penalty == "Defensive pass interference") %>% 
  select(target_name, pass_depth, season) %>% 
  arrange(desc(pass_depth)) %>% 
  head(30)

DPI_21 %>% 
ggplot(aes(x = EPA, y = avg_DPI_oe))+
  geom_point(aes(size = DPI_plays), fill = DPI_21$team_color2, color = "black", shape = 21)+
  geom_hline(yintercept = mean(DPI_21$avg_DPI_oe), color = "red", lty = "dashed")+
  geom_vline(xintercept = mean(DPI_21$EPA), color = "red", lty = "dashed")+
  geom_text_repel(aes(label = passer_name), color = "black", size = 6)+
  geom_smooth(method = 'lm', se = FALSE, color = 'black', alpha = 0.75)+
  theme_fivethirtyeight()+
  labs(title = "Quarterbacks DPI over expected and their career EPA/Play",
       caption = 'By: Arjun Menon | @arjunmenon100 | Data: PFF',
       subtitle = glue::glue("2011-2020 | Minimum 30 DPIs drawn | Dot size is # of DPIs"))+
  theme(axis.title = element_text(size = 16)) + ylab("DPI over Expected") + xlab("Career EPA/Play")+
  theme(axis.text = element_text(size = 14))+
  theme(legend.position = "none")+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 20, hjust = 0.5))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=8))+  
  scale_y_continuous(breaks = scales::pretty_breaks(n=11))
ggsave('DPIoe.png', width = 14, height = 10, dpi = "retina")
  
DPI_21 %>%
  ungroup() %>% 
  select(EPA, avg_DPI_oe) %>% 
  cor(use = "complete.obs") %>% 
  round(2)

DPI_WR <- DPI_projs %>%
  mutate(DPI_oe = DPI - exp_DPI) %>%
  mutate(offense = case_when(
    offense == "SD" ~ "LAC",
    offense == "BLT" ~ "BAL",
    offense == "OAK" ~ "LV",
    offense == "HST" ~ "HOU",
    offense == "SL" ~ "LA",
    offense == "CLV" ~ "CLE", 
    offense == "ARZ" ~ "ARI",
    TRUE ~ offense
  )) %>% 
  group_by(target_name) %>%  
  summarize(plays = n(),
            avg_DPI_oe = mean(DPI_oe)*100,
            EPA = mean(EPA[penalty == "Defensive pass interference"]),
            air_yards = sum(pass_depth[penalty == "Defensive pass interference"]),
            Mean_air_yards = mean(pass_depth[penalty == "Defensive pass interference"]),
            offense = first(offense)) %>% 
  filter(plays > 700) %>% 
  mutate(
    target_name = case_when(
      target_name == "Odell Beckham Jr." ~ "Odell Beckham",
      target_name == "Marvin Jones Jr." ~ "Marvin Jones",
      target_name == "Allen Robinson II" ~ "Allen Robinson",
      TRUE ~ target_name
    )
  )

future::plan("multisession")
heights <- nflfastR::fast_scraper_roster(2011:2021) 

DPI_WR <- DPI_WR  %>% 
  left_join(heights, by = c('target_name' = 'full_name')) %>% 
group_by(target_name) %>% 
  slice(1) %>%   
  mutate(team = case_when(
    team == "SD" ~ "LAC",
    team == "BLT" ~ "BAL",
    team == "OAK" ~ "LV",
    team == "HST" ~ "HOU",
    team == "SL" ~ "LA",
    team == "CLV" ~ "CLE", 
    team == "ARZ" ~ "ARI",
    TRUE ~ team
  )) %>% 
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

DPI_WR <- DPI_WR %>% 
  mutate(
    PL_heights = case_when(
      height == '5-8' ~ '68',
      height == '5-9' ~ '69',
      height == '5-10' ~ '70',
      height == '5-11' ~ '71',
      height == '6-0' ~ '72',
      height == '6-1' ~ '73',
      height == '6-2' ~ '74',
      height == '6-3' ~ '75',
      height == '6-4' ~ '76',
      height == '6-5' ~ '77',
      height == '6-6' ~ '78',
      TRUE ~ height
    )
  ) %>% 
  filter(!is.na(PL_heights))

DPI_WR %>% 
  ggplot(aes(x = PL_heights, y = avg_DPI_oe))+
  geom_point(aes(size = plays), fill = DPI_WR$team_color2, color = "black", shape = 21)+
  geom_text_repel(aes(label = target_name), color = "black", size = 5)+
  geom_smooth(method = 'lm', se = FALSE, color = 'black', alpha = 0.75)+
  theme_fivethirtyeight()+
  labs(title = "Wide Receivers DPI over expected and their height",
       caption = 'By: Arjun Menon | @arjunmenon100 | Data: PFF',
       subtitle = glue::glue("2011-2020 | Minimum 700 targets | Dot size is # of targets"))+
  theme(axis.title = element_text(size = 16)) + ylab("DPIs over Expected") + xlab("Player height (inches)")+
  theme(axis.text = element_text(size = 14))+
  theme(legend.position = "none")+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 20, hjust = 0.5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=7))
ggsave('DPIWR.png', width = 14, height = 10, dpi = "retina")

