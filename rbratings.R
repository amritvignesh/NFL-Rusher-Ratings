library(nflfastR)
library(dplyr)
library(lme4)
library(gt)
library(gtExtras)

rush_pbp <- load_pbp(seasons = 2024) %>% 
  filter(season_type == "REG", rush_attempt == 1, qb_scramble == 0, 
         qb_dropback == 0, !is.na(yards_gained)) %>%
  mutate(rusher_player_name = paste0(rusher_player_id, "_", posteam)) %>%
  select(yards_gained, yardline_100, quarter_seconds_remaining, half_seconds_remaining, qtr, down, ydstogo, shotgun,
         no_huddle, ep, wp, rusher_player_name, posteam, defteam) %>%
  filter(!is.na(down))

mixed_model <- lmer(
  yards_gained ~ yardline_100 + quarter_seconds_remaining + half_seconds_remaining + 
    qtr + down + ydstogo + shotgun + no_huddle + ep + wp + 
    (1 | rusher_player_name) + (1 | posteam) + (1 | defteam), 
  data = rush_pbp
) # fixed effects are all situational factors, random effects are rusher, rest of offense, defense

summary(mixed_model)

rusher_effects <- as.data.frame(ranef(mixed_model)$rusher_player_name)
rusher_effects$rusher_player_name <- rownames(rusher_effects)
rownames(rusher_effects) <- NULL
colnames(rusher_effects)[1] <- "coef"
rusher_effects$posteam <- sub(".*_", "", rusher_effects$rusher_player_name)
rusher_effects$coef <- percent_rank(rusher_effects$coef) * 100 # based on percentile

carries <- rush_pbp %>%
  group_by(rusher_player_name, posteam) %>%
  summarize(carries = n())

carries <- carries %>%
  group_by(posteam) %>%
  arrange(-carries) %>%
  filter(row_number() <= 3) %>%
  mutate(rb_num = row_number())

carries$rb_num[which(carries$rusher_player_name == "00-0037258_HOU")] <- 2
carries$rb_num[which(carries$rusher_player_name == "00-0036414_HOU")] <- 3
# both akers and pierce have same amount of carries, however, pierce was viewed as the higher choice rb in the beginning of the season as per depth charts
  
rusher_effects <- rusher_effects %>% left_join(carries, by = c("rusher_player_name", "posteam")) %>%
  filter(!is.na(carries)) %>%
  mutate(rusher_player_name = sub("_.*", "", rusher_player_name))

rusher_effects$coef <- round(rusher_effects$coef)

rosters <- fast_scraper_roster(2024) %>%
  select(rusher_player_name = gsis_id, name = full_name, headshot = headshot_url)

rusher_effects <- rusher_effects %>% left_join(rosters, by = "rusher_player_name")

teams <- teams_colors_logos %>%
  select(team = team_abbr, logo = team_logo_espn)

rusher_effects <- rusher_effects %>% left_join(teams, by = c("posteam"="team"))

rb_ones <- rusher_effects %>% filter(rb_num == 1) %>% arrange(posteam) %>%
  select(logo, team = posteam, rb1_headshot = headshot, rb1 = name, rb1_carries = carries, rb1_coef = coef)

rb_twos <- rusher_effects %>% filter(rb_num == 2) %>% arrange(posteam) %>% mutate(rank = rank(-coef)) %>%
  select(rb2_headshot = headshot, rb2 = name, rb2_carries = carries, rb2_coef = coef)

rb_threes <- rusher_effects %>% filter(rb_num == 3) %>% arrange(posteam) %>% mutate(rank = rank(-coef)) %>%
  select(rb3_headshot = headshot, rb3 = name, rb3_carries = carries, rb3_coef = coef)

rb_df <- cbind(rb_ones, rb_twos, rb_threes)

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption_1 = gt_align_caption("Data from <b>nflverse</b>", "")
caption_2 = gt_align_caption("", "Amrit Vignesh | <b>@avsportsanalyst</b>   ||||")

table_1 <- rb_df %>% head(16) %>% gt() %>%
  gt_img_rows(columns = logo, height = 50) %>%
  gt_img_rows(columns = rb1_headshot, height = 50) %>%
  gt_img_rows(columns = rb2_headshot, height = 50) %>%
  gt_img_rows(columns = rb3_headshot, height = 50) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  gt_hulk_col_numeric(c(rb1_carries, rb2_carries, rb3_carries),
                      domain = c(15, 345)) %>%
  gt_hulk_col_numeric(c(rb1_coef, rb2_coef, rb3_coef),
                      domain = c(0, 100)) %>%
  cols_label(
    logo = "",
    team = md("**Team**"),
    rb1_headshot = "",
    rb1 = md("**RB1**"),
    rb1_carries = md("**Carries**"),
    rb1_coef = md("**Rating**"),
    rb2_headshot = "",
    rb2 = md("**RB2**"),
    rb2_carries = md("**Carries**"),
    rb2_coef = md("**Rating**"),
    rb3_headshot = "",
    rb3 = md("**RB3**"),
    rb3_carries = md("**Carries**"),
    rb3_coef = md("**Rating**")
  ) %>%
  tab_header(
    title = "2024 NFL Rushing Core Ratings   ",
    subtitle = md("*Based on **Random Effect Coefficient Percentiles** & *")
  ) %>% 
  tab_source_note(html(caption_1)) %>%
  opt_align_table_header(align = "right") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(team, rb1, rb1_coef, rb2, rb2_coef, rb3, rb3_coef)
    )
  ) 

table_2 <- rb_df %>% filter(row_number() > 16) %>% gt() %>%
  gt_img_rows(columns = logo, height = 50) %>%
  gt_img_rows(columns = rb1_headshot, height = 50) %>%
  gt_img_rows(columns = rb2_headshot, height = 50) %>%
  gt_img_rows(columns = rb3_headshot, height = 50) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  gt_hulk_col_numeric(c(rb1_carries, rb2_carries, rb3_carries),
                      domain = c(15, 345)) %>%
  gt_hulk_col_numeric(c(rb1_coef, rb2_coef, rb3_coef),
                      domain = c(0, 100)) %>%
  cols_label(
    logo = "",
    team = md("**Team**"),
    rb1_headshot = "",
    rb1 = md("**RB1**"),
    rb1_carries = md("**Carries**"),
    rb1_coef = md("**Rating**"),
    rb2_headshot = "",
    rb2 = md("**RB2**"),
    rb2_carries = md("**Carries**"),
    rb2_coef = md("**Rating**"),
    rb3_headshot = "",
    rb3 = md("**RB3**"),
    rb3_carries = md("**Carries**"),
    rb3_coef = md("**Rating**")
  ) %>%
  tab_header(
    title = "Based on Production Relative to Situational Factors",
    subtitle = md("*QBs Can Be Classified as **RB1-3** Based on Carries*")
  ) %>% 
  tab_source_note(html(caption_2)) %>%
  opt_align_table_header(align = "left") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(team, rb1, rb1_coef, rb2, rb2_coef, rb3, rb3_coef)
    )
  ) 

gtsave(table_1, "table_1.png", vwidth = 4000, vheight = 3000)
gtsave(table_2, "table_2.png", vwidth = 4000, vheight = 3000)


