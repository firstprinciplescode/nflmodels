library(nflreadr)
library(nflfastR)
library(sqldf)
library(openxlsx)
library(tidyr)
library(plyr)
library(dplyr)
library(tidyverse)
library(readxl)
library(flexclust)
library(factoextra)
library(NbClust)
library(xgboost)
library(caret)
library(caTools)
library(Metrics)
library(cvms)
library(ParBayesianOptimization)
library(doParallel)
library(stringr)
library(pROC)
library(olsrr)
library(stats)
library(ROCR)
library(gridExtra)
library(mgcv)


options(scipen = 999)

setwd("C:/Users/vflre/Downloads/NFL Models")

'%ni%' <- Negate('%in%')



ids_one <- load_rosters_weekly(season = c(2016:2024))

ids_one$team[which(ids_one$team == "ARI")] <- "ARZ"
ids_one$team[which(ids_one$team == "BAL")] <- "BLT"
ids_one$team[which(ids_one$team == "CLE")] <- "CLV"
ids_one$team[which(ids_one$team == "HOU")] <- "HST"
ids_one$team[which(ids_one$team == "OAK")] <- "LV"
ids_one$team[which(ids_one$team == "SD")] <- "LAC"

ids_one$week <- ifelse(ids_one$week == 18 & ids_one$season <= 2020, 28, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 19 & ids_one$season <= 2020, 29, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 19 & ids_one$season > 2020, 28, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 20 & ids_one$season <= 2020, 30, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 20 & ids_one$season > 2020, 29, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 21 & ids_one$season <= 2020, 32, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 21 & ids_one$season > 2020, 30, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 22, 32, ids_one$week)


#####
##### PULLING IN EVERYTHING I POSSIBLY CAN - PASSERS, RUSHERS, RECEIVERS
#####


rushing_df <- read.xlsx("df_rushing_stats.xlsx")

# HAS POSITION

receiver_scheme <- read.xlsx("df_receiver_scheme.xlsx")

# HAS POSITION

pff_fantasy_stats_one <- read.xlsx("df_fantasy_stats_all.xlsx")

# HAS POSITION

colnames(pff_fantasy_stats_one)[3] <- "franchise_id"

# HAS POSITION

receiver_depth <- read.xlsx("df_receiving_depth_all.xlsx")

# HAS POSITION

receiving_stats <- read.xlsx("df_receiving_all.xlsx")

# HAS POSITION

passing_concept <- read.xlsx("df_concept_all.xlsx")

# HAS POSITION

passing_tip <- read.xlsx("df_time_in_pocket_all.xlsx")

# HAS POSITION



pff_ids_one <- rbind(rushing_df %>% select(player, player_id, team, franchise_id, Week, Season, position), receiver_scheme %>% select(player, player_id, team, franchise_id, Week, Season, position), pff_fantasy_stats_one %>% select(player, player_id, team, franchise_id, Week, Season, position), receiver_depth %>% select(player, player_id, team, franchise_id, Week, Season, position), receiving_stats %>% select(player, player_id, team, franchise_id, Week, Season, position), passing_concept %>% select(player, player_id, team, franchise_id, Week, Season, position), passing_tip %>% select(player, player_id, team, franchise_id, Week, Season, position)) %>% distinct() %>% arrange(player_id, Season, Week)

# RECAST RB TO HB - ACTUALLY NOT THAT SIMPLE


####
####


pff_ids_one_grouped <- pff_ids_one %>%
  group_by(player_id, player, team, franchise_id, Week, Season) %>%
  summarize(
    positions = list(unique(position))  # Create a list of unique positions
  ) %>%
  ungroup() %>%
  mutate(
    position_one = map_chr(positions, ~ .x[1] %||% NA),  # Get the first position
    position_two = map_chr(positions, ~ .x[2] %||% NA),  # Get the second position
    position_three = map_chr(positions, ~ .x[3] %||% NA) # Get the third position (if exists)
  ) %>%
  select(-positions)  # Remove the list column

# View the result
pff_ids_one_grouped


transform_name <- function(name) {
  name_parts <- strsplit(name, " ")[[1]]
  first_name <- name_parts[1]
  rest_of_name <- paste(name_parts[-1], collapse = " ")
  initials <- paste0(substr(first_name, 1, 1), ".")
  transformed_name <- paste(initials, rest_of_name)
  gsub("\\s+", " ", transformed_name)
}

pff_ids_one_grouped$player_name_join <- sapply((sapply(pff_ids_one_grouped$player, transform_name)), str_trim)
pff_ids_one_grouped$player_name_join <- gsub("\\s+", "", pff_ids_one_grouped$player_name_join)
pff_ids_one_grouped$player_name_join <- gsub("(Jr\\.|Sr\\.|II|III|IV|V|VI)$", "", pff_ids_one_grouped$player_name_join)

sort(unique(pff_ids_one_grouped$player_name_join))
# CHECKS TO SEE HOW JR., SR. II, III ETC. ARE HANDLED
# LARRY ROUNTREE III --> L.ROUNTREE
# LAVISKA SHENAULT JR --> L.Shenault AND l.Shenault Jr. 
# DONALD PARHAM JR. --> D.PARHAM AND D.PARHAM JR.
# HENRY RUGGS III --> H.RUGGS
# KENNETH WALKER III --> K.WALKER

# II, III, IV, ETC. --> DON'T COUNT
# JR/SR DO STAY

# Let's remove these strings from both 


# GOTTA MAKE SURE TO MAKE THIS CHANGE IN COMBINED INFO


# AARON RODGERS - 00-0023459, AMARI RODGERS - 00-0036991
# DAMIEN WILLIAMS - 00-0030874, DARREL WILLIAMS - 00-0034301
# DAVID JOHNSON - 00-0032187, DUKE JOHNSON JR - 00-0032257 (NOTE THERE IS A DAVID JOHNSON ON PIT THAT IS ACCOUNTED FOR CORRECTLY)
# JAMAAL WILLIAMS - 00-0033948, JAMESON WILLIAMS - 00-0037240
# JARON BROWN - 00-0030300, JOHN BROWN - 00-0031051
# MAC JONES - 00-0036972, MARCUS JONES - 00-0037253

# WE ALREADY HAVE THE PLAYER_NAME IN COMBINED_PLAYERS_INFO - 


pff_ids_one_grouped %>% group_by(player_name_join, Week, Season, team) %>% dplyr::summarise(n = n_distinct(player_id)) %>% filter(n > 1)


pff_ids_one_grouped$player_name_join[which(pff_ids_one_grouped$player == "Aaron Rodgers")] <- "Aa.Rodgers"
pff_ids_one_grouped$player_name_join[which(pff_ids_one_grouped$player == "Amari Rodgers")] <- "Am.Rodgers"
pff_ids_one_grouped$player_name_join[which(pff_ids_one_grouped$player == "Damien Williams")] <- "Dam.Williams"
pff_ids_one_grouped$player_name_join[which(pff_ids_one_grouped$player == "Darrell Williams")] <- "Dar.Williams"
pff_ids_one_grouped$player_name_join[which(pff_ids_one_grouped$player == "David Johnson")] <- "Da.Johnson"
pff_ids_one_grouped$player_name_join[which(pff_ids_one_grouped$player == "Duke Johnson")] <- "Du.Johnson"
pff_ids_one_grouped$player_name_join[which(pff_ids_one_grouped$player == "Jamaal Williams")] <- "Jama.Williams"
pff_ids_one_grouped$player_name_join[which(pff_ids_one_grouped$player == "Jameson Williams")] <- "Jame.Williams"
pff_ids_one_grouped$player_name_join[which(pff_ids_one_grouped$player == "Jaron Brown")] <- "Ja.Brown"
pff_ids_one_grouped$player_name_join[which(pff_ids_one_grouped$player == "John Brown")] <- "Jo.Brown"
pff_ids_one_grouped$player_name_join[which(pff_ids_one_grouped$player == "Mac Jones")] <- "Mac.Jones"
pff_ids_one_grouped$player_name_join[which(pff_ids_one_grouped$player == "Marcus Jones")] <- "Mar.Jones"
pff_ids_one_grouped$player_name_join[which(pff_ids_one_grouped$player == "Roman Wilson")] <- "Ro.Wilson"

pff_ids_one_grouped$team[which(pff_ids_one_grouped$team == "OAK")] <- "LV"
pff_ids_one_grouped$team[which(pff_ids_one_grouped$team == "SD")] <- "LAC"


pff_ids_one_grouped$player_name_join <- sapply((sapply(pff_ids_one_grouped$player, transform_name)), str_trim)


ids_one <- ids_one %>% 
  mutate(player_abbr_join = sapply(paste0(first_name, " ", last_name), function(x) str_trim(transform_name(x))))
ids_one$player_abbr_join <- gsub(" ", "", ids_one$player_abbr_join)


combined_ids_one <- left_join(pff_ids_one_grouped, ids_one %>% mutate(pff_id = as.numeric(pff_id)) %>% select(pff_id, team, week, season, gsis_id), by = c("player_id" = "pff_id", "team" = "team", "Week" = "week", "Season" = "season"))
# THIS WORKS PRETTY WELL - SOMETIMES PFF_ID IS NULL. 
# TWO MORE STAGES - FIRST NAME / LAST NAME JOIN, THEN WE'LL HAVE TO CREATE OUR OWN FIRST NAME / LAST NAME

combined_ids_one %>% filter(is.na(gsis_id)) %>% arrange(player, Week, Season)

combined_ids_two <- left_join(combined_ids_one %>% filter(is.na(gsis_id)), ids_one %>% mutate(new_full_name = paste0(first_name, " ", last_name)) %>% select(new_full_name, team, week, season, gsis_id), by = c("player" = "new_full_name", "team" = "team", "Week" = "week", "Season" = "season"))
# AM TRANSITIONING TO ONLY JOIN THE DATA THAT 
# TWO: JUST FOR THOSE WITH NA GSIS_ID FROM ONE


# ON THIS JOIN - NO PFF_ID, NO FIRSTNAME + LASTNAME CALCULATION - FULL NAME AND HOPE FOR NO A.J. TYPE NICKNAMES 
combined_ids_three <- left_join(combined_ids_two %>% filter(is.na(gsis_id.x) & is.na(gsis_id.y)), ids_one %>% select(full_name, team, week, season, gsis_id), by = c("player" = "full_name", "team" = "team", "Week" = "week", "Season" = "season"))
# ALMOST THERE - LITERALLY JUST JR, SR, II, III, ETC.
# AT LEAST THOSE FUNCTIONS ARE DONE ALREADY LMAO


combined_ids_four_stage_one <- combined_ids_three %>% filter(is.na(gsis_id) & is.na(gsis_id.x) & is.na(gsis_id.y))
combined_ids_four_stage_one$player <- trimws(gsub("(Jr\\.|Sr\\.|II|III|IV|V|VI)$", "", combined_ids_four_stage_one$player))  
  
combined_ids_four_stage_one <- left_join(combined_ids_four_stage_one, ids_one %>% select(full_name, team, week, season, gsis_id), by = c("player" = "full_name", "team" = "team", "Week" = "week", "Season" = "season"))  

# SOME PLAYERS STILL MISSING. FUCK IT. MANUAL INTERPOLATION

combined_ids_four_stage_one %>% filter(is.na(gsis_id.x), is.na(gsis_id.y), is.na(gsis_id.x.x), is.na(gsis_id.y.y))

combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Zach Ertz' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0030061"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Matt Hazel' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0031315"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'KaVontae Turpin' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0037801"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == "De'Marcus Ayers" & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0032365"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Walter Powell' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0031268"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Rodney Williams' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0037451"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Michael Woods' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0037300"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Kyric McGowan' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0037172"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == "De'Von Achane" & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0039040"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Chris Brooks' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0038685"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Tank Dell' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0038977"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Adonai Mitchell' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0039890"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Alec Pierce' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0037664"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Alex Bachman' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0035602"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Alexander Mattison' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0035602"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Brian Thomas' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0039893"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Brock Bowers' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0039338"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Caleb Williams' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0039918"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Jamari Thrash' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0039379"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Kimani Vidal' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0039391"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Ladd McConkey' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0039915"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Rome Odunze' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0039919"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Scott Matlock' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0038614"
combined_ids_four_stage_one$gsis_id.y.y[which(combined_ids_four_stage_one$player == 'Tip Reiman' & is.na(combined_ids_four_stage_one$gsis_id.y.y))] <- "00-0039737"


# Step 1: Join the data frames
combined_ids_final <- combined_ids_one %>%
  full_join(combined_ids_two %>% select(player_id, team, franchise_id, Week, Season, gsis_id.y), by = c("player_id", "team", "franchise_id", "Week", "Season")) %>%
  full_join(combined_ids_three %>% select(player_id, team, franchise_id, Week, Season, gsis_id), by = c("player_id", "team", "franchise_id", "Week", "Season")) %>%
  full_join(combined_ids_four_stage_one %>% select(player_id, team, franchise_id, Week, Season, gsis_id.y.y), by = c("player_id", "team", "franchise_id", "Week", "Season"))


combined_ids_final <- combined_ids_final %>%
  group_by(player_id) %>%
  arrange(player_id) %>% # Ensure rows are properly ordered
  fill(gsis_id.x, gsis_id.y, gsis_id.y.y.x, gsis_id.y.y.y, .direction = "down") %>%
  ungroup()


combined_ids_final$gsis_id.y.y.y[which(combined_ids_final$player == 'Jaelon Darden')] <- "00-0036877"
combined_ids_final$gsis_id.y.y.y[which(combined_ids_final$player == 'Clayton Tune')] <- "00-0038582"

combined_ids_final %>% filter(is.na(gsis_id.x), is.na(gsis_id.y), is.na(gsis_id.y.y.y), is.na(gsis_id.y.y.x))


# Step 2: Coalesce the gsis_id columns
combined_ids_final <- combined_ids_final %>%
  mutate(
    gsis_id_final = coalesce(gsis_id.x, gsis_id.y, gsis_id.y.y.x, gsis_id.y.y.y)
  )

combined_ids_final %>% filter(is.na(gsis_id_final))

# Step 3: Remove extra columns and any duplicates
combined_ids_final <- combined_ids_final %>%
  select(-c(gsis_id.x, gsis_id.y, gsis_id.y.y.x, gsis_id.y.y.y)) %>%
  distinct()

# View the final result
combined_ids_final



