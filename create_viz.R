#Package import
library(tidyverse)
library(rvest)
library(gt)

#Season fixture results URLs from fbref.com
url <- c(
        "https://fbref.com/en/comps/9/1631/schedule/2017-2018-Premier-League-Scores-and-Fixtures#sched_1631_1::3",
        "https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures#sched_10728_1",
        "https://fbref.com/en/comps/9/3232/schedule/2019-2020-Premier-League-Scores-and-Fixtures#sched_3232_1",
        "https://fbref.com/en/comps/9/1889/schedule/2018-2019-Premier-League-Scores-and-Fixtures#sched_1889_1",
        "https://fbref.com/en/comps/9/1526/schedule/2016-2017-Premier-League-Scores-and-Fixtures#sched_1526_1",
        "https://fbref.com/en/comps/9/1467/schedule/2015-2016-Premier-League-Scores-and-Fixtures#sched_1467_1"
        )


#Do web-scraping
epl <- map_df(url, ~ {
  read_html(.) %>% html_element("table") %>% html_table()
})


#Filter out null rows and clean up column-names
epl_clean <- 
  epl %>% 
  filter(!is.na(Wk)) %>% 
  janitor::clean_names()
 
#Correct column formats and some additional cleaning 
epl_final <- 
  epl_clean %>% 
  mutate(date = lubridate::ymd(date), time = lubridate::hm(time), attendance = parse_number(attendance)) %>% 
  separate(score, c("home_goals", "away_goals")) %>% 
  mutate(across(c("home_goals", "away_goals"), parse_number)) %>% 
  select(wk:home, away, home_goals, away_goals, attendance, home_xg=x_g_6, away_xg=x_g_8, -match_report, -notes) %>% 
  mutate(points = case_when(
    home_goals > away_goals ~ 3,
    home_goals < away_goals ~ 0,
    home_goals == away_goals ~ 1
  ))
  

#Define clubs of interest
top_clubs <- c(
  "Arsenal", 
  "Manchester City", 
  "Chelsea",
  "Manchester Utd",
  "Tottenham",
  "Liverpool",
  "Everton",
  "Leicester City"
  )

#Aggregation of pre-covid performance metrics
pre_covid <- epl_final %>%
  filter(date < as.Date("2020-04-01"), home %in% top_clubs) %>% 
  group_by(home) %>% 
  summarise(
    pc_attendance = mean(attendance, na.rm = TRUE), 
    pc_home_goals = mean(home_goals),
    pc_home_xg = mean(home_xg),
    pc_points = mean(points),
    pc_winpct = sum(home_goals > away_goals) / n()
    ) %>% 
  ungroup()
 
#Aggregation of post-COVID performance metrics
covid <- epl_final %>%
  filter(date > as.Date("2020-04-01"), home %in% top_clubs) %>% 
  group_by(home) %>% 
  summarise(
    home_goals = mean(home_goals, na.rm = TRUE),
    home_xg = mean(home_xg, na.rm = TRUE),
    points = mean(points, na.rm = TRUE),
    winpct = sum(home_goals > away_goals, na.rm = TRUE) / n()
  ) %>% 
  ungroup()
 
 #This CSV contains URL links to EPL team logo thumbnails on premierleague.com
 logos <- read_csv('epl_logo_urls.csv')


#Join pre/post-COVID metrics into final dataframe 
final <- inner_join(covid, pre_covid) %>% inner_join(logos, by=c("home"="team"))




#Build our table

final_tab <- final %>% 
  select(url, home, home_goals, home_xg, winpct, pc_home_goals, pc_home_xg, pc_winpct, pc_attendance) %>% 
  arrange(-home_goals) %>% 
  gt() %>% 
  #Color cells where post-COVID win% is higher than pre-COVID green.
  tab_style(
    style = list(
      cell_fill(color = "#52eb34", alpha = 0.3)
    ),
    locations = cells_body(
      columns = vars(winpct),
      rows = winpct > pc_winpct
    )
  ) %>% 
  #Color cells green where post-COVID home_goals, home_xg are improved
  tab_style(
    style = list(
      cell_fill(color = "#52eb34", alpha = 0.3)
    ),
    locations = cells_body(
      columns = vars(home_goals, home_xg),
      rows = 2:3
    )
  ) %>% 
  #Add border separating post-COVID & pre-COVID sections
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = vars(pc_home_goals)
      )
    )
  ) %>% 
  #Add in premier league team logos
  text_transform(
    locations = cells_body(columns = vars(url)),
    fn = function(x) {
      web_image(
        url = x,
        height = 30
      )
    }
  ) %>% 
  fmt_number(
    columns = vars(pc_attendance),
    use_seps = TRUE,
    decimals = 0
  ) %>% 
  fmt_number(
    columns = vars(home_goals, home_xg,  pc_home_goals, pc_home_xg),
    decimals = 2
  ) %>% 
  fmt_percent(
    columns = vars(winpct, pc_winpct),
    decimals = 0
  ) %>% 
  tab_spanner(
    label = "Post-COVID (AVG)",
    columns = vars(home_goals, home_xg, winpct)
  ) %>% 
  tab_spanner(
    label = "Pre-Covid (AVG)",
    columns = vars(pc_home_goals, pc_home_xg, pc_winpct, pc_attendance)
  ) %>% 
  tab_source_note(
    source_note = "Table: @DesiGoonerMD | Data: fbref.com"
  ) %>% 
  tab_header(
    title = "How has lack of fans due to COVID impacted home performances?",
    subtitle = "Premier League"
  ) %>% 
  tab_footnote(
    footnote = "Based on results from June 17, 2020 - March 21, 2021",
    locations = cells_column_spanners(spanners = "Post-COVID (AVG)")
  ) %>% 
  tab_footnote(
    footnote = "Based on results from August 8, 2015 - March 9, 2020",
    locations = cells_column_spanners(spanners = "Pre-Covid (AVG)")
  ) %>% 
  cols_label(
    url = "",
    home = "",
    home_goals = "Home Goals",
    home_xg = "Home XG",
    winpct = "Win %",
    pc_home_goals = "Home Goals",
    pc_home_xg = "Home XG",
    pc_winpct = "Win %",
    pc_attendance = "Attendance"
  ) %>% 
  espnscrapeR::gt_theme_538() #Use fivethirtheight theme

#Save our table as png file for sharing
gtsave(final_tab, "C:/scripts/epl_home_advantage/final_table.png", zoom=2)
  
