source("src/get-player-data.R")
source("src/get-match-data.R")
source("src/get-hero-data.R")
source("src/calc-exp-summary.R")

library(tidyverse)
library(knitr)

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

# Calculate bingo square stats
bingo_stats <- data.frame(
  square = as.character(),
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
  square = "Divine Intervention",
  description = "At least 1 Godlike kill streak in a match",
  probability = kill_streak(matches_odota, 9, 1),
  requirement = 29
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Divine Intervention",
  description = "At least 2 Godlike kill streaks in a match",
  probability = kill_streak(matches_odota, 9, 2),
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Divine Retribution",
  description = "A player achieves a Beyond Godlike kill streak in a match",
  probability = kill_streak(matches_odota, 10, 1),
  requirement = 21
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Mega Mega Kill",
  description = "At least 4 Mega kill streaks in a match",
  probability = kill_streak(matches_odota, 5, 4),
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Monstrous Mayhem",
  description = "At least 1 Monster kill streak in a match",
  probability = kill_streak(matches_odota, 8, 1),
  requirement = 36
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Monstrous Mayhem",
  description = "At least 2 Monster kill streaks in a match",
  probability = kill_streak(matches_odota, 8, 2),
  requirement = 10
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Something Wicked",
  description = "At least 2 Wicked Sick kill streaks in a match",
  probability = kill_streak(matches_odota, 7, 2),
  requirement = 21
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Something Wicked",
  description = "At least 3 Wicked Sick kill streaks in a match",
  probability = kill_streak(matches_odota, 7, 3),
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Truly Unstoppable",
  description = "At least 4 Unstoppable kill streaks in a match",
  probability = kill_streak(matches_odota, 6, 4),
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Truly Unstoppable",
  description = "At least 3 Unstoppable kill streaks in a match",
  probability = kill_streak(matches_odota, 6, 3),
  requirement = 11
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
  square = "Ultra Cool",
  description = "At least 1 Ultra kill occurs in a match",
  probability = multi_kill(matches_odota, 4, 1),
  requirement = 7
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
  square = "Blink and You'll Miss It",
  description = "At least 5 blink daggers are purchased in a single match",
  probability = item_purchase(matches_odota, "blink", 5),
  requirement = 7
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Dagon Enthusiast",
  description = "Dagon Level 5 is purchased in a match",
  probability = item_purchase(matches_odota, "dagon_5", 1),
  requirement = 6
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Kingmaker",
  description = "At least 3 BKBs are purchased in a match",
  probability = item_purchase(matches_odota, "black_king_bar", 3),
  requirement = 27
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Kingmaker",
  description = "At least 5 BKBs are purchased in a match",
  probability = item_purchase(matches_odota, "black_king_bar", 5),
  requirement = 6
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
  square = "Caped Crew",
  description = "Team has at least 2 caped heroes",
  probability = hero_pick(matches_odota, heroes, "caped_crew", 2, "lower"),
  requirement = 24
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Caped Crew",
  description = "Team has at least 3 caped heroes",
  probability = hero_pick(matches_odota, heroes, "caped_crew", 3, "lower"),
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Flock Jocks",
  description = "Team has at least 2 heroes with wings",
  probability = hero_pick(matches_odota, heroes, "flock_jocks", 2, "lower"),
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Friendly Fire",
  description = "Team has at least 2 fiery heroes",
  probability = hero_pick(matches_odota, heroes, "friendly_fire", 2, "lower"),
  requirement = 18
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Fuzzy Wuzzy",
  description = "Team has at least 2 fuzzy heroes",
  probability = hero_pick(matches_odota, heroes, "fuzzy_wuzzy", 2, "lower"),
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Girl Power",
  description = "Team has at least 2 female heroes",
  probability = hero_pick(matches_odota, heroes, "girl_power", 2, "lower"),
  requirement = 27
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Girl Power",
  description = "Team has at least 3 female heroes",
  probability = hero_pick(matches_odota, heroes, "girl_power", 3, "lower"),
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Gruesome Grills",
  description = "Team has at least 2 heroes with bad teeth",
  probability = hero_pick(matches_odota, heroes, "gruesome_grills", 2, "lower"),
  requirement = 6
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Gym Rats",
  description = "Team has at least 2 heroes that have nice pecs",
  probability = hero_pick(matches_odota, heroes, "gym_rats", 2, "lower"),
  requirement = 12
)
bingo_stats <- bingo_stats %>% add_row(
  square = "High Flyers",
  description = "Team has at least 2 flying heroes",
  probability = hero_pick(matches_odota, heroes, "high_flyers", 2, "lower"),
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Huggable Heroes",
  description = "Team has at least 2 cute heroes",
  probability = hero_pick(matches_odota, heroes, "huggable_heroes", 2, "lower"),
  requirement = 8
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Legged Legion",
  description = "A team has at least 14 legs",
  probability = hero_pick(matches_odota, heroes, "legs", 14, "lower"),
  requirement = 17
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Lock Horns",
  description = "Team has at least 2 horned heroes",
  probability = hero_pick(matches_odota, heroes, "lock_horns", 2, "lower"),
  requirement = 6
)
bingo_stats <- bingo_stats %>% add_row(
  square = "No Leg Strategy",
  description = "Team has at most 6 legs",
  probability = hero_pick(matches_odota, heroes, "legs", 6, "upper"),
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Parental Unit",
  description = "Team has at least 2 heroes that are parents",
  probability = hero_pick(matches_odota, heroes, "parental_unit", 2, "lower"),
  requirement = 5
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Spider Bitten",
  description = "Team has at least 2 heroes that are arachnophobic",
  probability = hero_pick(matches_odota, heroes, "spider_bitten", 2, "lower"),
  requirement = 9
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Undeadly Duo",
  description = "Team has at least 2 undead heroes",
  probability = hero_pick(matches_odota, heroes, "undeadly_duo", 2, "lower"),
  requirement = 5
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
bingo_stats <- bingo_stats %>% add_row(
  square = "Double Down",
  description = "At least 1 kill occurs with an Amplify Damage Rune in a match",
  probability = rune_kills(matches_stratz, "DOUBLE_DAMAGE", 1),
  requirement = 27
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Double Down",
  description = "At least 2 kills occur with an Amplify Damage Rune in a match",
  probability = rune_kills(matches_stratz, "DOUBLE_DAMAGE", 2),
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Arcane Ruined",
  description = "At least 2 kills occur with an Arcane Rune in a match",
  probability = rune_kills(matches_stratz, "ARCANE", 2),
  requirement = 8
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Die On My Shield",
  description = "At least 1 kill occurs with a Shield Rune in a match",
  probability = rune_kills(matches_stratz, "SHIELD", 1),
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Die On My Shield",
  description = "At least 2 kills occur with a Shield Rune in a match",
  probability = rune_kills(matches_stratz, "SHIELD", 2),
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "The Quick and the Dead",
  description = "At least 1 kill occurs with a Haste Rune in a match",
  probability = rune_kills(matches_stratz, "HASTE", 1),
  requirement = 16
)
bingo_stats <- bingo_stats %>% add_row(
  square = "The Quick and the Dead",
  description = "At least 2 kills occur with a Haste Rune in a match",
  probability = rune_kills(matches_stratz, "HASTE", 2),
  requirement = NA
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
  square = "Indestructible",
  description = "A winning team has a player with zero deaths",
  probability = indestructible(matches_odota),
  requirement = NA
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
  square = "Peacemaker",
  description = "A winning team has a player with zero kills",
  probability = peacemaker(matches_odota),
  requirement = NA
)

endangered_species <- function(matches, threshold) {
  results <- c()
  for (match in matches) {
    metric <- 0
    for (player in match$players) {
      metric <- metric + player$courier_kills
    }
    if (metric >= threshold) result <- 1 else result <- 0
    results <- c(results, result)
  }
  return(calc_exp_summary(results))
}
bingo_stats <- bingo_stats %>% add_row(
  square = "Endangered Species",
  description = "At least 3 couriers die in a match",
  probability = endangered_species(matches_odota, 3),
  requirement = 24
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Endangered Species",
  description = "At least 5 couriers die in a match",
  probability = endangered_species(matches_odota, 5),
  requirement = 7
)

end_a_friend <- function(matches, heroes, threshold = FALSE) {
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
    
    if (threshold) {
      if (metric >= threshold) result <- 1 else result <- 0
    } else {
      result <- metric
    }
    results <- c(results, metric)
  }
  return(calc_exp_summary(results))
}
bingo_stats <- bingo_stats %>% add_row(
  square = "End A Friend",
  description = "At least 1 hero is denied in a match",
  probability = end_a_friend(matches_odota, heroes, 1),
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "End A Friend",
  description = "A hero is denied",
  probability = end_a_friend(matches_odota, heroes, FALSE),
  requirement = 15
)

controlled_demolition <- function(matches, heroes, threshold = FALSE) {
  results <- c()
  for (match in matches) {
    radiant <- c()
    dire <- c()
    metric <- 0
    for (pick in match$picks_bans) {
      if (pick$is_pick == TRUE) {
        hero <- heroes %>% filter(hero_id == pick$hero_id) %>% pull(hero_name)
        if (pick$team == 0) radiant <- c(radiant, hero)
        if (pick$team == 1) dire <- c(dire, hero)
      }
    }
    
    for (objective in match$objectives) {
      if (objective$type == "building_kill") {
        deny <- (grepl("^npc_dota_goodguys_tower", objective$key) &&
          objective$unit %in% radiant)
        if (deny) metric <- metric + 1
      }
    }
    
    if (threshold) {
      if (metric >= threshold) result <- 1 else result <- 0
    } else {
      result <- metric
    }
    results <- c(results, metric)
  }
  return(calc_exp_summary(results))
}
bingo_stats <- bingo_stats %>% add_row(
  square = "Controlled Demolition",
  description = "At least 1 tower is denied in a match",
  probability = controlled_demolition(matches_odota, heroes, 1),
  requirement = 11
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Controlled Demolition",
  description = "A tower is denied",
  probability = controlled_demolition(matches_odota, heroes, FALSE),
  requirement = 14
)

ward_cleaver <- function(matches, threshold) {
  results <- c()
  for (match in matches) {
    metric <- 0
    for (player in match$players) {
      if ("npc_dota_observer_wards" %in% names(player$killed)) {
        metric <- metric + player$killed$npc_dota_observer_wards
      }
    }
    if (metric >= threshold) result <- 1 else result <- 0
    results <- c(results, result)
  }
  return(calc_exp_summary(results))
}
bingo_stats <- bingo_stats %>% add_row(
  square = "Ward Cleaver",
  description = "At least 25 observer wards dewarded",
  probability = ward_cleaver(matches_odota, 25),
  requirement = 25
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Ward Cleaver",
  description = "At least 30 observer wards dewarded",
  probability = ward_cleaver(matches_odota, 30),
  requirement = 14
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Ward Cleaver",
  description = "At least 35 observer wards dewarded",
  probability = ward_cleaver(matches_odota, 30),
  requirement = 6
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
  square = "Smoking Kills",
  description = "At least 6 kills occur after use of Smoke of Deceit",
  probability = smoking_kills(matches_stratz, 6),
  requirement = 32
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Smoking Kills",
  description = "At least 8 kills occur after use of Smoke of Deceit",
  probability = smoking_kills(matches_stratz, 8),
  requirement = 20
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Smoking Kills",
  description = "At least 9 kills occur after use of Smoke of Deceit",
  probability = smoking_kills(matches_stratz, 9),
  requirement = 16
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Smoking Kills",
  description = "At least 10 kills occur after use of Smoke of Deceit",
  probability = smoking_kills(matches_stratz, 10),
  requirement = 12
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
  square = "Fast and Furious",
  description = "A match ends in less than 23 minutes",
  probability = fast_and_furious(matches_odota, 23),
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Fast and Furious",
  description = "A match ends in less than 25 minutes",
  probability = fast_and_furious(matches_odota, 25),
  requirement = 12
)

firster_first_blood <- function(matches) {
  results <- c()
  for (match in matches) {
    if (match$first_blood_time <= 0) result <- 1 else result <- 0
    results <- c(results, result)
  }
  return(calc_exp_summary(results))
}
bingo_stats <- bingo_stats %>% add_row(
  square = "Firster First Blood",
  description = "First blood occurs before the horn",
  probability = firster_first_blood(matches_odota),
  requirement = 16
)

fully_sharded <- function(matches) {
  results <- c()
  for (match in matches) {
    result <- 0
    metric <- c()
    for (player in match$players) {
      metric <- c(metric, player$aghanims_shard)
    }
    if (sum(metric[1:5]) == 5) result <- result + 1
    if (sum(metric[6:10]) == 5) result <- result + 1
    results <- c(results, result)
  }
  return(calc_exp_summary(results))
}
bingo_stats <- bingo_stats %>% add_row(
  square = "Fully Sharded",
  description = "All members of a team have Aghanim's Shards",
  probability = fully_sharded(matches_odota),
  requirement = 6
)

# Unknown stats
bingo_stats <- bingo_stats %>% add_row(
  square = "Arbor Ardor",
  description = "At least 200 trees cut in a match",
  probability = NA,
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Arbor Ardor",
  description = "At least 300 trees cut in a match",
  probability = NA,
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Arbor Ardor",
  description = "At least 350 trees cut in a match",
  probability = NA,
  requirement = NA
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Catch a Fight",
  description = "At least 3 kills occur after use of a Twin Gate",
  probability = NA,
  requirement = 7
)
bingo_stats <- bingo_stats %>% add_row(
  square = "Fountain Diver",
  description = "At least 1 player killed in their own fountain",
  probability = NA,
  requirement = 15
)
bingo_stats <- bingo_stats %>% add_row(
  square = "No Rune For You",
  description = "A rune is denied",
  probability = NA,
  requirement = 30
)
bingo_stats <- bingo_stats %>% add_row(
  square = "No Rune For You",
  description = "At least 2 runes are denied in a match",
  probability = NA,
  requirement = 6
)

# Calculate the expected results
expected_matches <- 20*2.5
expected_results <- bingo_stats %>%
  mutate(
    expectation = round(probability * expected_matches, 2),
    probability = round(probability * 100, 2),
    completion = round((expectation / requirement) * 100, 2)
  ) %>%
  select(
    square, description, probability, expectation, requirement, completion
  )

# Print the table
expected_results %>%
  arrange(square, description) %>%
  kable(
    format = "pipe",
    digits = 2,
    col.names = c(
      "Square Name", 
      "Square Description", 
      "Prob (%)",
      "Exp",
      "Req", 
      "Comp (%)"
    ),
    align = "llrrrr",
    caption = "The probabilities and expected outcomes for bingo squares"
  ) %>%
  gsub("NA", "??", .) %>%
  print()
