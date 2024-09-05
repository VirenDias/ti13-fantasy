source("src/get-player-data.R")
source("src/get-match-data.R")
source("src/get-hero-data.R")
source("src/calc-exp-summary.R")

library(tidyverse)

# Get data
teams_elim <- scan("data/teams_elim.csv", quiet = TRUE)
players <- get_player_data(league_id) %>% filter(!(team_id %in% teams_elim))
heroes <- get_hero_data()

match_ids <- get_match_ids(players$player_id)
match_ids_black <- scan(
  "data/matches/match_ids_black.csv", 
  quiet = TRUE
)
match_ids <- setdiff(match_ids, match_ids_black)
matches_odota <- get_match_odota_data(match_ids)
matches_stratz <- get_match_stratz_data(match_ids)

# Calculate bingo card stats
bingo_stats <- data.frame(
  card = as.character(),
  description = as.character(),
  probability = as.numeric(),
  requirement = as.numeric()
)

# Calculate kill streak related stats
kill_streak <- function(matches, streak, threshold) {
  results <- c()
  for (match in matches) {
    metric <- 0
    for (player in match$players) {
      if (streak %in% names(player$kill_streaks)) {
        metric <- metric + player$kill_streaks[[as.character(streak)]]
      }
    }
    if (metric >= threshold) {
      results <- c(results, 1)
    } else {
      results <- c(results, 0)
    }
  }
  return(calc_exp_summary(results))
}

bingo_stats <- bingo_stats %>% add_row(
  card = "Divine Intervention",
  description = "At least 2 Godlike streaks in a match",
  probability = kill_streak(matches_odota, 9, 2),
  requirement = 8
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Divine Retribution",
  description = "A player achieves a Beyond Godlike kill streak in a match",
  probability = kill_streak(matches_odota, 10, 1),
  requirement = 45
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Mega Mega Kill",
  description = "At least 4 Mega kill streaks in a match",
  probability = kill_streak(matches_odota, 5, 4),
  requirement = 25
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Something Wicked",
  description = "At least 3 Wicked Sick kill streaks in a match",
  probability = kill_streak(matches_odota, 7, 3),
  requirement = 7
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Truly Unstoppable",
  description = "At least 4 Unstoppable kill streaks in a match",
  probability = kill_streak(matches_odota, 6, 4),
  requirement = 5
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Truly Unstoppable",
  description = "At least 3 Unstoppable kill streaks in a match",
  probability = kill_streak(matches_odota, 6, 3),
  requirement = 21
)

# Calculate multi kill related stats
multi_kill <- function(matches, multi, threshold) {
  results <- c()
  for (match in matches) {
    metric <- 0
    for (player in match$players) {
      if (multi %in% names(player$multi_kills)) {
        metric <- metric + player$multi_kills[[as.character(multi)]]
      }
    }
    if (metric >= threshold) {
      results <- c(results, 1)
    } else {
      results <- c(results, 0)
    }
  }
  return(calc_exp_summary(results))
}

bingo_stats <- bingo_stats %>% add_row(
  card = "Ultra Cool",
  description = "At least 1 Ultra kill occurs in a match",
  probability = multi_kill(matches_odota, 4, 1),
  requirement = 16
)

# Calculate item purchase related stats
item_purchase <- function(matches, item, threshold) {
  results <- c()
  for (match in matches) {
    metric <- 0
    for (player in match$players) {
      for (purchase in player$purchase_log) {
        if (purchase$key == item) metric <- metric + 1
      }
    }
    if (metric >= threshold) result <- 1 else result <- 0
    results <- c(results, result)
  }
  return(calc_exp_summary(results))
}

bingo_stats <- bingo_stats %>% add_row(
  card = "Blink and You'll Miss It",
  description = "At least 5 blink daggers are purchased in a single match",
  probability = item_purchase(matches_odota, "blink", 5),
  requirement = 11
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Dagon Enthusiast",
  description = "Dagon Level 5 is purchased in a match",
  probability = item_purchase(matches_odota, "dagon_5", 1),
  requirement = 12
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Kingmaker",
  description = "At least 3 BKBs are purchased in a match",
  probability = item_purchase(matches_odota, "black_king_bar", 3),
  requirement = 47
)

# Calculate hero pick related stats
hero_pick <- function(matches, heroes, category, threshold, upper_lower) {
  results <- c()
  for (match in matches) {
    team_1 <- c()
    team_2 <- c()
    result <- 0
    for (pick in match$picks_bans) {
      if (pick$is_pick == TRUE) {
        if (pick$team == 0) team_1 <- c(team_1, pick$hero_id)
        if (pick$team == 1) team_2 <- c(team_2, pick$hero_id)
      }
    }
    
    metric_1 <- heroes %>% 
      filter(hero_id %in% team_1) %>%
      pull(!!category) %>%
      sum()
    metric_2 <- heroes %>% 
      filter(hero_id %in% team_2) %>%
      pull(!!category) %>%
      sum()
    
    if (upper_lower == "upper") {
      if (metric_1 <= threshold) result <- result + 1
      if (metric_2 <= threshold) result <- result + 1
    }
    if (upper_lower == "lower") {
      if (metric_1 >= threshold) result <- result + 1
      if (metric_2 >= threshold) result <- result + 1
    }
    results <- c(results, result)
  }
  
  return(calc_exp_summary(results))
}

bingo_stats <- bingo_stats %>% add_row(
  card = "Caped Crew",
  description = "Team has at least 2 caped heroes",
  probability = hero_pick(matches_odota, heroes, "caped_crew", 2, "lower"),
  requirement = 52
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Caped Crew",
  description = "Team has at least 3 caped heroes",
  probability = hero_pick(matches_odota, heroes, "caped_crew", 3, "lower"),
  requirement = 8
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Flock Jocks",
  description = "Team has at least 2 heroes with wings",
  probability = hero_pick(matches_odota, heroes, "flock_jocks", 2, "lower"),
  requirement = 23
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Friendly Fire",
  description = "Team has at least 2 fiery heroes",
  probability = hero_pick(matches_odota, heroes, "friendly_fire", 2, "lower"),
  requirement = 36
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Fuzzy Wuzzy",
  description = "Team has at least 2 fuzzy heroes",
  probability = hero_pick(matches_odota, heroes, "fuzzy_wuzzy", 2, "lower"),
  requirement = 19
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Girl Power",
  description = "Team has at least 3 female heroes",
  probability = hero_pick(matches_odota, heroes, "girl_power", 3, "lower"),
  requirement = 11
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Gym Rats",
  description = "Team has at least 2 heroes that have nice pecs",
  probability = hero_pick(matches_odota, heroes, "gym_rats", 2, "lower"),
  requirement = 26
)
bingo_stats <- bingo_stats %>% add_row(
  card = "High Flyers",
  description = "Team has at least 2 flying heroes",
  probability = hero_pick(matches_odota, heroes, "high_flyers", 2, "lower"),
  requirement = 41
)
bingo_stats <- bingo_stats %>% add_row(
  card = "No Leg Strategy",
  description = "Team has at most 6 legs",
  probability = hero_pick(matches_odota, heroes, "no_leg_strategy", 6, "upper"),
  requirement = 9
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Lock Horns",
  description = "Team has at least 2 horned heroes",
  probability = hero_pick(matches_odota, heroes, "lock_horns", 2, "lower"),
  requirement = 13
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Parental Unit",
  description = "Team has at least 2 heroes that are parents",
  probability = hero_pick(matches_odota, heroes, "parental_unit", 2, "lower"),
  requirement = 11
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Spider Bitten",
  description = "Team has at least 2 heroes that are arachnophobic",
  probability = hero_pick(matches_odota, heroes, "spider_bitten", 2, "lower"),
  requirement = 15
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Undeadly Duo",
  description = "Team has at least 2 undead heroes",
  probability = hero_pick(matches_odota, heroes, "undeadly_duo", 2, "lower"),
  requirement = 11
)

# Calculate rune kills related stats
rune_kills <- function(matches, rune, threshold) {
  results <- c()
  for (match in matches) {
    metric <- 0
    for (player in match$match$players) {
      rune_events <- bind_rows(player$playbackData$runeEvents)
      for (kill_event in player$playbackData$killEvents) {
        if (kill_event$isRuneEffected) {
          rune_type <- rune_events %>% 
            filter(time <= kill_event$time) %>%
            arrange(time) %>%
            tail(1) %>%
            pull(rune)
          if (rune_type == rune) metric <- metric + 1
        }
      }
      
    }
    if (metric >= threshold) result <- 1 else result <- 0
    results <- c(results, result)
  }
  return(calc_exp_summary(results))
}
# rune_kills <- function(matches, rune, threshold) {
#   results <- c()
#   for (match in matches) {
#     result <- 0
#     for (player in match$match$players) {
#       if (length(player$playbackData$killEvents) > 0) {
#         kill_events <- bind_rows(player$playbackData$killEvents)
#         for (rune_event in player$playbackData$runeEvents) {
#           if (rune_event$rune == rune && rune_event$action == "PICKUP") {
#             rune_event$start <- rune_event$time
#             rune_event$duration <- switch (
#               rune,
#               "DOUBLE_DAMAGE" = 45,
#               "ARCANE" = 50,
#               "HASTE" = 22 + 3*floor((rune_event$time - 360)/720),
#               "SHIELD" = 75
#             )
#             rune_event$end <- rune_event$start + rune_event$duration
#             kills <- kill_events %>% 
#               filter(time >= rune_event$start, time <= rune_event$end) %>%
#               nrow()
#             if (kills >= threshold) result <- 1
#           }
#         }
#       }
#     }
#     results <- c(results, result)
#   }
#   return(calc_exp_summary(results))
# }
bingo_stats <- bingo_stats %>% add_row(
  card = "Double Down",
  description = "At least 2 kills occur with an Amplify Damage Rune in a match",
  probability = rune_kills(matches_stratz, "DOUBLE_DAMAGE", 2),
  requirement = 17
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Arcane Ruined",
  description = "At least 2 kills occur with an Arcane Rune in a match",
  probability = rune_kills(matches_stratz, "ARCANE", 2),
  requirement = 15
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Die On My Shield",
  description = "At least 1 kill occurs with a Shield Rune in a match",
  probability = rune_kills(matches_stratz, "SHIELD", 1),
  requirement = 37
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Die On My Shield",
  description = "At least 2 kills occur with a Shield Rune in a match",
  probability = rune_kills(matches_stratz, "SHIELD", 2),
  requirement = 6
)
bingo_stats <- bingo_stats %>% add_row(
  card = "The Quick and the Dead",
  description = "At least 2 kills occur with a Haste Rune in a match",
  probability = rune_kills(matches_stratz, "HASTE", 2),
  requirement = 6
)

# Calculate miscellaneous stats
indestructible <- function(matches) {
  results <- c()
  for (match in matches) {
    result <- 0
    for (player in match$players) {
      if (player$win == 1 && player$deaths == 0) result <- 1
    }
    results <- c(results, result)
  }
  return(calc_exp_summary(results))
}
bingo_stats <- bingo_stats %>% add_row(
  card = "Indestructible",
  description = "A winning team has a player with zero deaths",
  probability = indestructible(matches_odota),
  requirement = 26
)

peacemaker <- function(matches) {
  results <- c()
  for (match in matches) {
    result <- 0
    for (player in match$players) {
      if (player$win == 1 && player$kills == 0) result <- 1
    }
    results <- c(results, result)
  }
  return(calc_exp_summary(results))
}
bingo_stats <- bingo_stats %>% add_row(
  card = "Peacemaker",
  description = "A winning team has a player with zero kills",
  probability = peacemaker(matches_odota),
  requirement = 9
)

end_a_friend <- function(matches, heroes, threshold) {
  results <- c()
  for (match in matches) {
    team_1 <- c()
    team_2 <- c()
    metric <- 0
    for (pick in match$picks_bans) {
      if (pick$is_pick == TRUE) {
        if (pick$team == 0) team_1 <- c(team_1, pick$hero_id)
        if (pick$team == 1) team_2 <- c(team_2, pick$hero_id)
      }
    }
    
    for (player in match$players) {
      for (killer_name in names(player$killed_by)) {
        if (killer_name %in% heroes$hero_name) {
          killer_id <- heroes %>% 
            filter(hero_name == killer_name) %>% 
            pull(hero_id)
          deny <- (killer_id %in% team_1 && player$hero_id %in% team_1) ||
            (killer_id %in% team_2 && player$hero_id %in% team_2)
          if (deny) metric <- metric + player$killed_by[[killer_name]]
        }
      }
    }
    
    if (metric >= threshold) result <- 1 else result <- 0
    results <- c(results, result)
  }
  return(calc_exp_summary(results))
}
bingo_stats <- bingo_stats %>% add_row(
  card = "End A Friend",
  description = "At least 1 hero is denied in a match",
  probability = end_a_friend(matches_odota, heroes, 1),
  requirement = 7
)

ward_cleaver <- function(matches) {
  results <- c()
  for (match in matches) {
    result <- 0
    for (player in match$players) {
      if ("npc_dota_observer_wards" %in% names(player$killed)) {
        result <- result + player$killed$npc_dota_observer_wards
      }
    }
    results <- c(results, result)
  }
  return(calc_exp_summary(results))
}
bingo_stats <- bingo_stats %>% add_row(
  card = "Ward Cleaver",
  description = "At least 30 observer wards dewarded",
  probability = ward_cleaver(matches_odota),
  requirement = 25*30
)

smoking_kills <- function(matches, threshold) {
  results <- c()
  for (match in matches) {
    metric <- 0
    for (player in match$match$players) {
      for (kill_event in player$playbackData$killEvents) {
        if (kill_event$isSmoke) metric <- metric + 1
      }
    }
    if (metric >= threshold) result <- 1 else result <- 0
    results <- c(results, result)
  }
  return(calc_exp_summary(results))
}
bingo_stats <- bingo_stats %>% add_row(
  card = "Smoking Kills",
  description = "At least 6 kills occur after use of Smoke of Deceit",
  probability = smoking_kills(matches_stratz, 6),
  requirement = 55
)
bingo_stats <- bingo_stats %>% add_row(
  card = "Smoking Kills",
  description = "At least 9 kills occur after use of Smoke of Deceit",
  probability = smoking_kills(matches_stratz, 9),
  requirement = 29
)

fast_and_furious <- function(matches, duration) {
  results <- c()
  for (match in matches) {
    if (match$duration <= duration*60) result <- 1 else result <- 0
    results <- c(results, result)
  }
  return(calc_exp_summary(results))
}
bingo_stats <- bingo_stats %>% add_row(
  card = "Fast and Furious",
  description = "A match ends in less than 23 minutes",
  probability = fast_and_furious(matches_odota, 23),
  requirement = 19
)

# Calculate the expectation
expected_matches <- 68
bingo_stats <- bingo_stats %>%
  mutate(expectation = probability * expected_matches, .after = probability) %>%
  mutate(completion = expectation / requirement)
