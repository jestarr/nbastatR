#--- Supporting Functions ----
#---- .get_data_bref_player_seasons ----

.get_data_bref_player_seasons <-
  function(seasons = 2019,
           table = "advanced",
           only_totals = TRUE,
           return_message = TRUE) {
    # Character Vector of Current Season Using Lubridate of Sys.Time
    current_season <- .get_current_season()

    urls <-
      .generate_years_urls(table = table, seasons = seasons)
    .parse_player_season_safe <-
      purrr::possibly(.parse_player_season, tibble())

    all_data <-
      urls %>%
      map_df(function(x) {
        .parse_player_season_safe(url = x, return_message = return_message)
      })

    all_data <-
      all_data %>%
      dplyr::select(-dplyr::matches("urlData")) %>%
      tidyr::unite(slugPlayerSeason, slugPlayerBREF, yearSeason, remove = F)


    all_data <-
      all_data %>%
      mutate(isSeasonCurrent = slugSeason == current_season) %>%
      select(typeData, slugSeason, isSeasonCurrent, everything())

    all_data <-
      all_data %>%
      mutate(yearSeason = slugSeason %>% substr(1, 4) %>% as.numeric() + 1)


    all_data <-
      all_data %>%
      arrange(yearSeason)

    df_players_teams <-
      all_data %>%
      filter(slugTeamBREF != "TOT") %>%
      group_by(slugPlayerBREF, slugSeason) %>%
      dplyr::summarise(slugTeamsBREF = str_c(slugTeamBREF, collapse = " | "),
                       countTeamsPlayerSeason = length(slugTeamsBREF)) %>%
      ungroup()

    all_data <-
      all_data %>%
      left_join(df_players_teams,  by = c("slugSeason", "slugPlayerBREF"))

    if (only_totals) {
      all_data <-
        all_data %>%
        group_by(yearSeason, slugPlayerBREF) %>%
        filter(countGames == max(countGames)) %>%
        ungroup()
    }
    gc()

    all_data <-
      all_data %>%
      nest(-c(typeData, slugSeason, yearSeason), .key = dataTable)
    all_data
  }





.get_current_season <-
function() {
  current_year <- lubridate::year(Sys.Date())
  current_month <- lubridate::month(Sys.Date())

  season_slug <- case_when(
    current_month %>% between(6, 12) ~ str_c(current_year, "-", substr(current_year + 1, 3, 4)),
    TRUE ~ str_c(current_year - 1, "-", substr(current_year, 3, 4))
  )
  season_slug
}

.generate_years_urls <-
  memoise::memoise(function(table = "per_game",
                            seasons = 1951:2017) {
    tables <-
      c('per_game', 'advanced', 'totals', 'per_minute', 'per_poss')

    if (!table %in% tables) {
      stop(str_c("Tables can only be: " , tables %>% paste(collapse = ', ')))
    }

    urls <-
      list('http://www.basketball-reference.com/leagues/NBA_',
           seasons,
           '_',
           table,
           '.html') %>%
      purrr::reduce(paste0)
    return(urls)
  })





#---- Arguments ----
seasons = 2018:2019
tables = c('advanced', 'totals')
include_all_nba = F
only_totals = TRUE
nest_data = FALSE
assign_to_environment = TRUE
widen_data = TRUE
join_data = TRUE
return_message = TRUE




#---- Main Function ----
bref_players_stats <-
  function(seasons = NULL,
           tables = c('advanced', 'totals'),
           include_all_nba = F,
           only_totals = TRUE,
           nest_data = FALSE,
           assign_to_environment = TRUE,
           widen_data = TRUE,
           join_data = TRUE,
           return_message = TRUE) {
    if (length(seasons) == 0) {
      stop("Enter season(s)")
    }
    tables2 <-
      tables %>% str_replace_all("\\ ", "_") %>% str_to_lower()
    # 1
    .get_data_bref_player_seasons_safe <-
      purrr::possibly(.get_data_bref_player_seasons, tibble())

    all_data <-
      tables %>%
      map_df(function(x) {
        .get_data_bref_player_seasons_safe(
          table = x,
          seasons = seasons,
          only_totals = only_totals,
          return_message = return_message
        )
      })

    all_data <-
      assign_bref_data(
        data = all_data,
        type = "Players",
        widen_data = widen_data,
        include_all_nba = include_all_nba,
        join_data = join_data,
        nest_data = nest_data,
        assign_to_environment = assign_to_environment
      )

    all_data <-
      all_data %>%
      mutate(yearSeason = slugSeason %>% substr(1, 4) %>% as.numeric() + 1)

    all_data
  }
