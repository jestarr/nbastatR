library(curl)
library(jsonlite)


generate_season_slug <-
  function(season = 2018) {
    season_start <- season - 1
    season_end_slug <- season %>% substr(3,4)

    glue::glue("{season_start}-{season_end_slug}") %>%
      as.character()

  }

resolve_nba_names <- function(json_names) {
  df_nba_names <-
    dictionary_nba_names()

  json_names %>%
    map_chr(function(name){
      no_name <-
        df_nba_names %>%
        filter(nameNBA == name) %>%
        nrow() == 0

      if (no_name) {
        glue::glue("Missing {name} in dictionary") %>% cat(fill = T)
        return(name)
      }
      df_nba_names %>%
        filter(nameNBA == name) %>%
        pull(nameActual) %>%
        unique() %>%
        .[[1]]
    })

}

munge_nba_data <- function(data) {
  if (data %>% tibble::has_name("datetimeBirth")) {
    data <-
      data %>%
      mutate(datetimeBirth = datetimeBirth %>%  readr::parse_datetime() %>% as.Date())
  }

  if (data %>% tibble::has_name("timeGame")) {
    data <-
      data %>%
      tidyr::separate(timeGame, into = c("hours", "minutes"), sep = "\\:") %>%
      mutate_at(c("hours", "minutes"),
                funs(. %>% as.numeric())) %>%
      mutate(lengthGameMinutes = (hours * 60) + minutes) %>%
      dplyr::select(-one_of(c("hours", "minutes")))
  }

  if (data %>% tibble::has_name("minutes")) {
    if (data$minutes %>% str_count("\\:") %>% sum(na.rm = T) > 0) {
      data <- data %>%
        tidyr::separate(minutes, into = c("min", "seconds"), sep = "\\:") %>%
        mutate_at(c("min", "seconds"),
                  funs(. %>% as.numeric())) %>%
        mutate(seconds = seconds / 60,
               minExact = min + seconds) %>%
        dplyr::select(-c(min, seconds)) %>%
        dplyr::select(one_of(c("idGame", "descriptionComment", "minExact")), everything()) %>%
        suppressWarnings()
    }
  }

  char_names <- data %>% dplyr::select(dplyr::matches(char_words())) %>% names()

  num_names <-
    data %>% dplyr::select(-one_of(char_names)) %>% names()

  data <-
    data %>%
    mutate_at(num_names,
              funs(. %>% as.numeric())) %>%
    suppressWarnings()

  if (data %>% tibble::has_name("fga") && data %>%  tibble::has_name("fg3a")) {
    data <-
      data %>%
      mutate(fg2m = fgm - fg3m,
             fg2a = fga - fg3a,
             pctFG2 = if_else( fg2a > 0 , fg2m / fg2a, 0 )
      )
  }

  if (data %>% tibble::has_name("slugMatchup")){
    data <-
      data %>%
      mutate(locationGame = case_when(slugMatchup %>% str_detect("@") ~
                                        "A",
                                      T ~ "H")) %>%
      tidyr::separate(
        slugMatchup,
        into = c("remove", "slugOpponent"),
        sep = c("vs.|@"),
        remove = F
      ) %>%
      dplyr::select(-remove) %>%
      mutate_if(is.character,
                funs(. %>% str_trim())) %>%
      dplyr::select(slugSeason:outcomeGame, locationGame, everything())
  }

  if (data %>% tibble::has_name("groupStartPosition")){
    data <-
      data %>%
      mutate(isStarter = !is.na(groupStartPosition)) %>%
      dplyr::select(dplyr::matches("id|name|slug|city|is"), everything())
  }

  if (data %>% tibble::has_name("dateGame")) {
    if (data$dateGame %>% str_detect("T") %>% sum(na.rm = T) > 0) {
      data <-
        data %>%
        mutate(dateGame = dateGame %>% substr(1,10) %>% lubridate::ymd())
    }
  }

  if (data %>% tibble::has_name("dateGameLast")) {
    if (data$dateGameLast %>% str_detect("T") %>% sum(na.rm = T) > 0) {
      data <-
        data %>%
        mutate(dateGameLast = dateGameLast %>% substr(1,10) %>% lubridate::ymd())
    } else {
      data <-
        data %>%
        mutate(dateGameLast = dateGameLast %>% lubridate::mdy())
    }
  }
  if (data %>% tibble::has_name("slugScore")) {
    data <-
      data %>%
      tidyr::separate(slugScore, into = c("scoreHome", "scoreAway"), sep = "\\ - ", remove = F) %>%
      mutate_at(c("scoreHome", "scoreAway"),
                funs(. %>% as.numeric())) %>%
      mutate(slugTeamLeading = case_when(marginScore == 0 ~ "Tie",
                                         marginScore < 0 ~ "Away",
                                         TRUE ~ "Home"))
  }

  if (data %>% tibble::has_name("nameGroup") && data %>% tibble::has_name("nameGroupValue")) {
    data <-
      data %>%
      dplyr::select(-nameGroup) %>%
      dplyr::rename(typeFilter = nameGroupValue)
  }

  if (data %>% tibble::has_name("timeQuarter")) {
    data <-
      data %>%
      tidyr::separate(
        "timeQuarter",
        into = c("minuteRemainingQuarter", "secondsRemainingQuarter"),
        sep = "\\:",
        remove = F
      ) %>%
      mutate_at(c("minuteRemainingQuarter", "secondsRemainingQuarter"),
                funs(. %>% as.numeric())) %>%

      mutate(
        minuteGame = ((numberPeriod - 1) * 12) + (12 - minuteRemainingQuarter) + (((
          60 - secondsRemainingQuarter
        ) / 60) - 1),
        timeRemaining = 48 - ((numberPeriod - 1) * 12) - (12 - minuteRemainingQuarter) -
          ((60 - secondsRemainingQuarter) / 60 - 1)
      ) %>%
      dplyr::select(idGame:numberPeriod, minuteGame, timeRemaining, everything())
  }

  if (data %>% tibble::has_name("dateGameLastPlayed")) {
    data <-
      data %>%
      mutate(dateGameLastPlayed = dateGameLastPlayed %>% substr(1,10) %>% lubridate::ymd())
  }

  if (data %>% tibble::has_name("slugRecordTeam")){
    data <-
      data %>%
      tidyr::separate(slugRecordTeam,
                      sep = "\\-",
                      into = c("winsTeam", "lossesTeam"),
                      remove = F) %>%
      mutate_at(c("winsTeam", "lossesTeam"),
                funs(. %>% as.numeric())) %>%
      mutate(countGamesTeam = winsTeam + lossesTeam,
             pctWinTeam = winsTeam / (countGamesTeam)) %>%
      dplyr::select(idGame, slugRecordTeam,countGamesTeam, pctWinTeam, everything())
  }
  data <-
    data %>%
    mutate_if(is.character,
              funs(str_trim)) %>%
    mutate_if(is.character,
              funs(ifelse(. == "", NA, .)))

  logicial_names <-
    data %>% dplyr::select(dplyr::matches("^has[A-Z]|^is[A-Z]")) %>% names()

  if (logicial_names %>% length() > 0) {
    data <-
      data %>%
      mutate_at(logicial_names,
                funs(. %>% as.numeric() %>% as.logical()))
  }

  id_names <-
    data %>% dplyr::select(dplyr::matches("idTeam", "idPlayer")) %>% names()

  if (id_names %>% length() > 0) {
    data <-
      data %>%
      mutate_at(id_names,
                funs(. %>% as.numeric()))
  }

  if (data %>% has_name("teamName") & !data %>% has_name("cityTeam")) {
    data <-
      data %>%
      dplyr::rename(nameTeam = teamName)
  }

  if (data %>% tibble::has_name("namePlayerOnOff")) {
    assign_nba_players()
    data <-
      data %>%
      dplyr::select(-one_of("namePlayerOnOff")) %>%
      left_join(
        df_dict_nba_players %>% select(idPlayerOnOff = idPlayer,
                                       namePlayerOnOff = namePlayer)
      ) %>%
      dplyr::select(dplyr::matches("type[A-Z]|id[A-Z]|name[A-Z]"),
                    everything()) %>%
      suppressMessages()

    data <-
      data %>%
      filter(!namePlayerOnOff %>% is.na())

    data <-
      data %>%
      dplyr::rename(typeFilter = namePlayerOnOff)
  }

  if (data %>% tibble::has_name("fg3a") && data %>% tibble::has_name("fg3m")) {
    data <-
      data %>%
      mutate(pctFG3 = fg3m / fg3a)
  }

  if (data %>% tibble::has_name("namePlayerPasser")) {
    assign_nba_players()
    data <-
      data %>%
      dplyr::select(-one_of("namePlayerPasser")) %>%
      dplyr::rename(idPlayerPasserPassTo = idPlayerPasser) %>%
      left_join(
        df_dict_nba_players %>% select(idPlayerPasserPassTo = idPlayer,
                                       namePlayerPasserPassTo = namePlayer)
      ) %>%
      dplyr::select(dplyr::matches("type[A-Z]|id[A-Z]|name[A-Z]"),
                    everything()) %>%
      suppressMessages()
  }

  if (data %>% tibble::has_name("namePlayerPassTo")) {
    assign_nba_players()
    data <-
      data %>%
      dplyr::rename(idPlayerPasserPassTo = idPlayerPasser) %>%
      dplyr::select(-one_of("namePlayerPassTo")) %>%
      left_join(
        df_dict_nba_players %>% select(idPlayerPasserPassTo = idPlayer,
                                       namePlayerPasserPassTo = namePlayer)
      ) %>%
      dplyr::select(dplyr::matches("type[A-Z]|id[A-Z]|name[A-Z]"),
                    everything()) %>%
      suppressMessages()
  }


  data <-
    data %>%
    dplyr::select(-dplyr::matches("CIF")) %>%
    dplyr::select(-one_of(c("orderSort", "idLeague", "namePlayerLastFirst", "dateGameLastPlayed"))) %>%
    suppressWarnings()

  if (data %>% tibble::has_name("idTeam1")) {
    data <-
      data %>%
      dplyr::rename(idTeam = idTeam1)
  }

  if (data %>% has_name("yearSeasonFirst")) {
    data <-
      data %>%
      mutate(yearSeasonFirst = yearSeasonFirst + 1,
             yearSeasonLast = yearSeasonLast + 1)
  }

  data <-
    data %>%
    mutate_at(
      .vars = data %>% select(dplyr::matches("^pts|^blk")) %>% names(),
      funs(. %>% as.numeric())
    )

  data <-
    data %>%
    dplyr::select(which(colMeans(is.na(.)) < 1))

  char_names <-
    data %>% select_if(is.character) %>% names()

  data <-
    data %>%
    dplyr::select(one_of(char_names), everything()) %>%
    dplyr::select(
      dplyr::matches(
        "slugTable|group[A-Z]|type[A-Z]|mode[A-Z]|id[A-Z]|name[A-Z]|year|slug[A-Z]|number[A-Z]|date|outcome|^url|gp|gs|minutes[A-Z]|passes|^fg|^pct[A-Z]"
      ),
      everything()
    )

  data <-
    data %>%
    mutate_if(is.numeric,
              funs(ifelse(. %>% is.nan(), 0 , .)))

  data
}


char_words <-
  function(words = c("name[A-Z]", "date[A-Z]", "slug[A-Z]", "outcome[A-Z]", "team[A-Z]", 'height[A-Z]', 'result[A-Z]', "segment[A-Z]", "range[A-Z]", "vs[A-Z]", "mode[A-Z]", "category[A-Z]", "record[A-Z]", "^url[A-Z]", "code[A-Z]",
                     "description", "city", "time[A-Z]", "nickname[A-Z]", "group[A-Z]", "location[A-Z]", "zone[A-Z]", "type[A-Z]")){
    words %>% stringr::str_c(collapse = "|")
  }

.get_season_metric_league_leaders <-
  function(season = 2018,
           metric = "pts",
           season_type = "Regular Season",
           mode =  "Per48",
           return_message = TRUE) {

    # Arguments
    season = 2018
    metric = "pts"
    season_type = "Regular Season"
    mode =  "Per48"
    return_message = TRUE


    slug_season <-
      season %>% generate_season_slug()

    modes <- c(
      "Totals",
      "PerGame",
      "Per48"

    )

    if (!mode %>% str_to_lower() %in% str_to_lower(modes)) {
      mode_slugs <- modes %>% str_c(collapse='\n')
      stop(glue::glue("Modes can only be {mode_slugs}"))
    }
    scope_slug <- "S"
    json_url <-
      glue::glue("https://stats.nba.com/stats/leagueLeaders?LeagueID=00&PerMode={mode}&Scope={scope_slug}&Season={slug_season}&SeasonType={season_type}&StatCategory={metric}") %>%
      as.character() %>%
      URLencode()

    if (return_message) {
      glue::glue("Acquiring {metric} {mode} league leaders in the {slug_season} season") %>% cat(fill = T)
    }

    json <-
      json_url %>%
      curl() %>%
      fromJSON(simplifyDataFrame = T)

    # Take Character Vector and Resolve NBA Names
    actual_names <- json$resultSet$headers %>% resolve_nba_names()
    df_params <- json$parameters %>% flatten_df()
    actual_params <- names(df_params) %>% resolve_nba_names()
    df_params <-
      df_params %>%
      purrr::set_names(actual_params) %>%
      mutate(numberTable = 1)

    data <-
      json$resultSet$rowSet %>%
      data.frame(stringsAsFactors = F) %>%
      dplyr::as_tibble() %>%
      purrr::set_names(actual_names) %>%
      # Separate Function that Cleans up all data
      munge_nba_data() %>%
      mutate(numberTable = 1)

    data %>%
      left_join(df_params) %>%
      dplyr::select(one_of(actual_params), everything()) %>%
      select(-one_of("numberTable", "idLeague")) %>%
      suppressMessages()


  }
