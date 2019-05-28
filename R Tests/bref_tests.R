#---- Arguments ----
seasons = 2018:2019
tables = c('advanced', 'totals')
include_all_nba = F
only_totals = TRUE
nest_data = FALSE
assign_to_environment = FALSE
widen_data = TRUE
join_data = TRUE
return_message = TRUE




#---- Main Function ----
# Uses Functions
# .get_data_bref_player_seasons
# assign_bref_data

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
    tables <-
      tables %>% str_replace_all("\\ ", "_") %>% str_to_lower()


    .get_data_bref_player_seasons_safe <-
      # 1
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

    # Assigns Data Frame according to criteria in Arguments
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
#--- Supporting Functions ----
#---- # 1 .get_data_bref_player_seasons ----
.get_data_bref_player_seasons <-
  function(seasons = 2019,
           table = "advanced",
           only_totals = TRUE,
           return_message = TRUE) {
    # Character Vector of Current Season Using Lubridate of Sys.Time and Season Slug
    # 1a
    current_season <- .get_current_season()

    urls <-
      #1b
      .generate_years_urls(table = table, seasons = seasons)

    #1c parse_player_season
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





# Prints season_slug ex(2018-19)
.get_current_season <-
function() {
  current_year <- lubridate::year(Sys.Date())
  current_month <- lubridate::month(Sys.Date())
# if Current month is between 6 to 12, year shows as 2019-20, otherwise, 2018 - 19
  season_slug <- case_when(
    current_month %>% between(6, 12) ~ str_c(current_year, "-", substr(current_year + 1, 3, 4)),
    TRUE ~ str_c(current_year - 1, "-", substr(current_year, 3, 4))
  )
  season_slug
}

# Creates a list of URLS for Tables and Seasons
.generate_years_urls <- function(table = "per_game",
                            seasons = 1951:2017) {
    tables <-
      c('per_game', 'advanced', 'totals', 'per_minute', 'per_poss')

    if (!table %in% tables) {
      stop(str_c("Tables can only be: " , tables %>% paste(collapse = ', ')))
    }
  # Create list of URLS for
    urls <-
      list('http://www.basketball-reference.com/leagues/NBA_',
           seasons,
           '_',
           table,
           '.html') %>%
      purrr::reduce(paste0)
    return(urls)
  }


.parse_player_season <- function(url = "http://www.basketball-reference.com/leagues/NBA_1997_per_game.html",
             return_message = TRUE) {
      ## case_when

      page <-
        url %>%
        read_html()

      url_df <- url %>% httr::parse_url() %>% flatten_df()

      url_path <-
        url_df$path %>% str_replace_all(".html|leagues/NBA_", '')

      year_season_end <-
        url_path %>% str_split('\\_') %>% flatten_chr() %>% .[[1]] %>% as.character() %>% readr::parse_number()

      name_slug <-
        url_path %>%
        map_chr(function(x) {
          parts <-
            x %>% str_split('\\_') %>% flatten_chr()

          parts[2:length(parts)] %>% str_to_title() %>% str_c(collapse = '')
        })

      id_season <-
        list(year_season_end - 1, '-', year_season_end %>% str_sub(3, 4)) %>%
        purrr::reduce(paste0)

      players <-
        page %>%
        html_nodes('th+ .left a') %>%
        html_text()

      player_id <-
        page %>%
        html_nodes('th+ .left a') %>%
        html_attr('href') %>%
        str_replace_all('/players/', '')

      player_ids <-
        player_id %>%
        map_chr(function(x) {
          x %>%
            str_replace_all('.html', '') %>%
            str_split('/') %>%
            flatten_chr() %>%
            .[[2]]
        })

      df_players <-
        tibble(slugPlayerBREF = player_ids, namePlayer = players) %>%
        distinct() %>%
        mutate(numberPlayer = 1:n()) %>%
        select(numberPlayer, everything())

      df <-
        page %>%
        html_table() %>%
        .[[1]] %>%
        data.frame(stringsAsFactors = FALSE) %>%
        tbl_df() %>%
        dplyr::select(-dplyr::matches("Var"))

      df <-
        df %>%
        mutate_at(df %>% dplyr::select(-one_of(c(
          "Tm", "Player", "Pos"
        ))) %>% names(),
        funs(. %>% as.numeric())) %>%
        filter(!Rk %>% is.na()) %>%
        suppressWarnings()

      # df_players_unique <-
      #   df %>% group_by(Rk) %>% slice(1) %>%
      #   select(numberPlayer = Rk, namePlayer = Player) %>%
      #   ungroup()
      #
      # df <-
      #   df %>%
      #   dplyr::select(-dplyr::matches("Rk"))

      df_names <-
        get_bref_name_df()

      bref_names <-
        names(df)

      actual_names <-
        seq_along(bref_names) %>%
        map_chr(function(x) {
          actual <-
            df_names %>%
            filter(nameBREF == bref_names[x]) %>%
            .$nameActual

          if (actual == 'MinutesPlayed') {
            if (!name_slug == 'pergame') {
              actual <-
                str_c('total', actual)
            } else {
              actual <-
                str_c('pergame', actual)
            }
            return(actual)
          }

          if (actual %>% substr(1, 1) %in% LETTERS) {
            actual <-
              str_c(name_slug, actual)
          }
          return(actual)
        })


      df <-
        df %>%
        purrr::set_names(actual_names) %>%
        mutate(
          isHOFPlayer = namePlayer %>% str_detect('\\*'),
          namePlayer = namePlayer %>% str_replace_all('\\*', '')
        )

      df <-
        df %>%
        left_join(df_players) %>%
        distinct() %>%
        suppressMessages() %>%
        mutate(slugSeason = id_season,
               yearSeason = year_season_end,
               urlData = url) %>%
        dplyr::select(slugSeason, yearSeason,
                      slugPlayerBREF, everything())

      df <-
        df %>%
        mutate_at(df %>% dplyr::select(dplyr::matches("pct")) %>% names(),
                  funs(ifelse(. >= 1, . / 100, .))) %>%
        mutate(typeData = name_slug) %>%
        dplyr::select(typeData, everything())

      if (return_message) {
        list("parsed ", url) %>%
          purrr::reduce(paste0) %>%
          cat(fill = T)
      }
      df <- df %>%
        select(-one_of("numberPlayer"))

      df <- df %>%
        mutate_if(is.numeric, list(function(x){
          ifelse(is.na(x), 0, x)
        }))
      gc()
      df
    }


get_bref_name_df <- function() {
    tibble(
      nameBREF = c(
        "slugSeason",
        "yearSeason",
        "Rk",
        "idPlayer",
        "Player",
        "Pos",
        "Age",
        "Tm",
        "G",
        "GS",
        "MP",
        "FG",
        "FGA",
        "FG%",
        "3P",
        "3PA",
        "3P%",
        "2P",
        "2PA",
        "2P%",
        "eFG%",
        "FT",
        "FTA",
        "FT%",
        "ORB",
        "DRB",
        "TRB",
        "AST",
        "STL",
        "BLK",
        "TOV",
        "PF",
        "PS/G",
        "urlData",
        "PTS",
        "PER",
        "TS.",
        "X3PAr",
        "FTr",
        "ORB.",
        "DRB.",
        "TRB.",
        "AST.",
        "STL.",
        "BLK.",
        "TOV.",
        "USG.",
        "OWS",
        "DWS",
        "WS",
        "WS.48",
        "OBPM",
        "DBPM",
        "BPM",
        "VORP",
        "FG.",
        "X3P",
        "X3PA",
        "X3P.",
        "X2P",
        "X2PA",
        "X2P.",
        "eFG.",
        "FT.",
        "PS.G",
        "Team"

      ),
      nameActual = c(
        "slugSeason",
        "yearSeason",
        "numberPlayer",
        "slugPlayerBREF",
        "namePlayer",
        "slugPosition",
        "agePlayer",
        "slugTeamBREF",
        "countGames",
        "countGamesStarted",
        "minutes",
        "fgm",
        "fga",
        "pctFG",
        "fg3m",
        "fg3a",
        "pctFG3",
        "fg2m",
        "fg2a",
        "pctFG2",
        "pctEFG",
        "ftm",
        "fta",
        "pctFT",
        "orb",
        "drb",
        "trb",
        "ast",
        "stl",
        "blk",
        "tov",
        "pf",
        "pts",
        "urlData",
        "pts",
        "ratioPER",
        "pctTrueShooting",
        "pct3PRate",
        "pctFTRate",
        "pctORB",
        "pctDRB",
        "pctTRB",
        "pctAST",
        "pctSTL",
        "pctBLK",
        "pctTOV",
        "pctUSG",
        "ratioOWS",
        "ratioDWS",
        "ratioWS",
        "ratioWSPer48",
        "ratioOBPM",
        "ratioDBPM",
        "ratioBPM",
        "ratioVORP",
        "pctFG",
        "fg3m",
        "fg3a",
        "pctFG3",
        "fg2m",
        "fg2a",
        "pctFG2",
        "pctEFG",
        "pctFT",
        "pts",
        "nameTeam"
      )
    )
  }


assign_bref_data <- function(data,
                            type = "Players",
                            widen_data = TRUE,
                            include_all_nba = F,
                            join_data = TRUE,
                            nest_data = FALSE,
                            assign_to_environment = TRUE) {
    type_slug <-
      type %>% str_to_lower()

    if (!type_slug %in% c('players', 'teams')) {
      stop("Type can only be players or teams")
    }

    if (type_slug == "players") {
      data <-
        .assign.bref.players(
          all_data = data,
          widen_data = widen_data,
          join_data = join_data,
          include_all_nba = include_all_nba,
          assign_to_environment = assign_to_environment
        )
    }

    if (type_slug == "teams") {
      data <-
        .assign.bref.teams(
          all_data = data,
          widen_data = widen_data,
          join_data = join_data,
          assign_to_environment = assign_to_environment
        )
    }

    if (nest_data) {
      if (data %>% tibble::has_name("slugSeason")) {
        data <-
          data %>%
          mutate(typeBREFData = type) %>%
          nest(-c(slugSeason, typeBREFData, yearSeason), .key = dataSeason)
      }
    }
    data
  }

