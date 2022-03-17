#Data Prep

library(tidyverse)

drop_cols <- c(
  'player_url',
  'short_name',
  'dob',
  'player_positions',
  'skill_moves',
  'release_clause_eur',
  'team_jersey_number',
  'real_face',
  'nation_jersey_number',
  'team_jersey_number',
  'player_tags',
  'loaned_from',
  'joined',
  'player_traits',
  'body_type',
  'nation_position'
)

fwd_pos <- c('LS', 'RS', 'ST', 'LW', 'RW', 'CF', 'LF', 'RF')
mid_pos <-
  c('LAM',
    'CAM',
    'RAM',
    'LM',
    'LCM',
    'CM',
    'RCM',
    'RM',
    'LDM',
    'RDM',
    'CDM')
def_pos <- c('LWB', 'RWB', 'LB', 'LCB', 'CB', 'RCB', 'RB')

years <- 15:21

for (i in 1:length(years)) {
  year = as.character(years[i])
  filename <- paste('players_', year, '.csv', sep = "")
  
  pl <- read_csv(filename)
  pl <- pl[!(names(pl) %in% drop_cols)]
  pl <- pl[, 1:63]
  
  pl <- pl %>% filter(!team_position %in% c('RES', '', 'SUB')) %>% 
    filter(!is.na(team_position))
  
  pl <- pl %>% mutate(
    fwd = ifelse(team_position %in% fwd_pos, 1, 0),
    mid = ifelse(team_position %in% mid_pos, 1, 0),
    def = ifelse(team_position %in% def_pos, 1, 0),
    gk = ifelse(team_position == 'GK', 1, 0),
    year = strtoi(year),
    id = strtoi(paste(sofifa_id, year, sep = ''))
  ) %>% select(-sofifa_id) %>% select(-team_position)
  
  if(i == 1){
    pls <- pl
  }
  else{
    pls <- rbind(pls, pl)
  }
}

pls <- pls %>% mutate(
  club_name = ifelse(is.na(club_name), 'None', club_name),
  league_name = ifelse(is.na(league_name), 'None', league_name),
  league_rank = ifelse(is.na(league_rank), 0, league_rank),
  contract_valid_until = ifelse(is.na(contract_valid_until), 2000 + year, contract_valid_until),
  contract_length = contract_valid_until - 2000 - year
) %>% select(-contract_valid_until)

pls$pace[is.na(pls$pace)]<-round(mean(pls$pace,na.rm=TRUE))
pls$shooting[is.na(pls$shooting)]<-round(mean(pls$shooting,na.rm=TRUE))
pls$passing[is.na(pls$passing)]<-round(mean(pls$passing,na.rm=TRUE))
pls$dribbling[is.na(pls$dribbling)]<-round(mean(pls$dribbling,na.rm=TRUE))
pls$defending[is.na(pls$defending)]<-round(mean(pls$defending,na.rm=TRUE))
pls$physic[is.na(pls$physic)]<-round(mean(pls$physic,na.rm=TRUE))
pls$mentality_composure[is.na(pls$mentality_composure)]<-round(mean(pls$mentality_composure, 
                                                             na.rm=TRUE))
pls$defending_marking[is.na(pls$defending_marking)]<-round(mean(pls$defending_marking,
                                                                na.rm=TRUE))

pls[is.na(pls)] <- 0

#Converting to factors
pls$club_name <- as.factor(pls$club_name)
pls$nationality <- as.factor(pls$nationality)
pls$league_name <- as.factor(pls$league_name)
pls$preferred_foot <- as.factor(pls$preferred_foot)
pls$work_rate <- as.factor(pls$work_rate)

pls <- pls %>% mutate(value = value_eur + (52 * 5 * wage_eur)) %>%
  filter(value > 0) %>% mutate(ln_val = log(value))

pls <- pls %>% mutate(pos = case_when(fwd == 1 ~ 'FWD',
                                      mid == 1 ~ 'MID',
                                      def == 1 ~ 'DEF',
                                      gk == 1 ~ 'GK'))

save(pls, file = "players.RData")
