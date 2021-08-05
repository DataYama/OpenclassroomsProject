require(tidyverse)
require(zoo)





# estim win rate ----------------------------------------------------------



win_rate <- data.frame(
  win = c(0,0,0,1,1, 0,0,0,0,0, 0,1,0,0,1, 0, # 4/16
          1,0,0,1,1, 0,0,1,0,1, 0,1,1,0,0, 1,0,1, # 9/18
          0,1,0,0,1, 0,0,1,1,1, 1,0,0,0,1, 0,0,0,0, # 7/19
          1,0,1,1,0, 1,0,0,1,0, 1,1,0,0,1, 1,0,1,0, # 10/19
          0,0,0,1,1, 0,0,0,1,1, 1,0,0,0, # 5/14
          1,0,1,1,1, 0,1,1,1,0, 0,0,1,1, 1,1,0, # 11/18
          1,0,1,1,1, 1,1,1,1,0, 0, # 8/11
          0,1,1,0,1, 0,1,0,1,1, # 6/10
          0,1,0,1,0, 0,1,0,0,0, 1,0, # 4/12
          1,0,1,0,1, 1,1,0,0,0, 1,0,1,1, # 8/14
          0,0,1,0,1, 0,1,1,0,0, 0,0 # 4/12
          )
)

game_window <- 50

win_rate %>%
  mutate(
    win_cum = cumsum(win),
    game = row_number(),
    rate = win_cum/game,
    win_cum_roll = rollapply(win, game_window, sum, align = "right", na.pad = T),
    rate_roll = win_cum_roll/game_window,
  ) %>%
  ggplot(aes(game, rate)) +
  geom_line() + geom_point() +
  geom_line(aes(y = rate_roll), color = "red") +
  geom_point(aes(y = rate_roll), color = "red") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0,1,0.04)) +
  labs(y = 'Taux de victoire', x = 'Nombre de parties', 
       caption = "En rouge sur les 50 dernières parties")



# estim bootstrap ---------------------------------------------------------




game_nb <- 16
win_rate <- 0.46
days_nb <- 30
boot_nb <- 200

boot_result <- vector("integer", length = boot_nb)
for(i in 1:boot_nb){
  
  cat(i, " ")
  
  day <- vector("integer", length = days_nb)
  streak <- vector("integer", length = days_nb)
  
  for(j in 1:days_nb){
    
    tirage_raw <- data.frame(
      game = seq(1,game_nb,1),
      result = runif(game_nb, min = 0, max = 1)
    ) %>%
      mutate(win = ifelse(result >= (1 - win_rate), 1, 0))
    
    while(tirage_raw$win[nrow(tirage_raw)] == 1){
      tirage_raw <- tirage_raw %>%
        add_row(game = max(tirage_raw$game) + 1,
                result = runif(1, min = 0, max = 1)) %>%
        mutate(win = ifelse(result >= (1 - win_rate), 1, 0))
    }
    
    tirage_prep <- tirage_raw %>%
      mutate(streak = case_when(game == 1 & win == 1 ~ 1,
                                lag(win) == 0 & win == 1 ~ 1,
                                win == 0 ~ 0,
                                TRUE ~ 0)) %>%
      mutate(streak_cumsum = cumsum(streak)) %>%
      group_by(streak_cumsum) %>%
      mutate(win_cumul = cumsum(win)) %>%
      summarise(consecutive = max(win_cumul),
                .groups= "drop_last") %>%
      ungroup()
    
    day[j] = j
    streak[j] = max(tirage_prep$consecutive)
  }
  
  result <- data.frame(day, streak) %>%
    filter(streak >= 3)
  
  if(nrow(result) == 0){
    boot_result[i] = days_nb
  } else{
    boot_result[i] = min(result$day)
  }
}

boot_df <- data.frame(days_nb = boot_result)

ggplot(boot_df, aes(days_nb)) +
  stat_ecdf() +
  # geom_histogram(binwidth = 1) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0,1,0.04)) +
  labs(subtitle = "",
       x = "Evening number")



# finals ------------------------------------------------------------------


finals <- tribble(
  ~name,           ~win, ~loss,
  "Batteries",       12,     0,
  "Basket",          16,     3,
  "Fonte des glaces", 9,    10,
  "Hexagones",        9,     8,
  "Maudits",         11,     3,
  "Fall Ball",        9,     2,
  "Ca coule",         2,     0,
  "Grimpette",        1,     1,
  "Et que Ã§a saute",  1,     0,
  "Bazar royal",      1,     0
)

ggplot(finals, aes(reorder(name, win/(win+loss)), win/(win+loss))) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha=2/3, width = 0.8) +
  geom_text(aes(label = paste0(win, "/", win+loss), y = win/(win+loss)+0.05)) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0,0), limits = c(0,1.1)) +
  labs(y = "Taux de victoire", x = "Finale")





# new comment for modification comit









