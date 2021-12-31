epa_data2 <- epa_data %>% 
  select(game_id, play_id, penalty, EP, EPA)

epa_data3 <- epa_2021 %>% 
  select(game_id, play_id, penalty, EP, EPA)

epa_data3 <- epa_data3 %>% 
  left_join(passing_feed, by = c('game_id', 'play_id'))

passing_pbp <- passing_data %>% 
  left_join(epa_data2, by = c('game_id', 'play_id'))

passing_pbp <- rbind(passing_pbp, epa_data3)

third_down_pen <- passing_pbp %>% 
  filter(penalty == 'Defensive pass interference') %>% 
  group_by(target_name) %>% 
  summarise(
    EPA = round(sum(EPA, na.rm = T), 2),
    DPIs = n()
  ) %>% 
  arrange(desc(EPA)) %>% 
  ungroup() %>% 
  mutate(
    target_name = case_when(
      target_name == "Marvin Jones Jr." ~ "Marvin Jones",
      target_name == "Allen Robinson II" ~ "Allen Robinson",
      TRUE ~ target_name
    )
  ) %>% 
left_join(pictures, by = c('target_name' = 'full_name')) %>% 
  slice(-8)

pictures <- nflfastR::fast_scraper_roster(2021) %>% select(full_name, gsis_id, headshot_url)

library(gtExtras)

third_down_tbl <- third_down_pen %>% 
  select(target_name, headshot_url, EPA, DPIs) %>% 
  slice(1:10) %>% 
  gt() %>% 
  gt_theme_538() %>% 
  gt_merge_stack(col1 = EPA, col2 = DPIs, color = "#505056") %>%
  gt_img_rows(headshot_url) %>% 
  gt_color_rows(
    EPA:EPA, palette = c('#87d180', '#4e9f50'),
    use_paletteer = FALSE) %>%
  cols_align(
    align = "center",
    columns = c(target_name, headshot_url, EPA, DPIs)
  ) %>% 
  cols_label(
    target_name = "Wide Receiver",
    headshot_url = "",
    EPA = "Total EPA Generated") %>% 
  tab_header(
    title = "Most successful Wide Receivers at drawing pass interferences",
    subtitle = "2011-2021 | # of PI's under EPA"
  ) %>% 
  tab_source_note("Table: @arjunmenon100 | Data: PFF")
gtsave(third_down_tbl, "third_down_tbl_WR.png")
