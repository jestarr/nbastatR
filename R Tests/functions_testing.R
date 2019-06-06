#---- Basketball Reference ----

.get_data_bref_player_seasons <-
  function(seasons = 2019,
           table = "advanced",
           only_totals = TRUE,
           return_message = TRUE) {
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

.parse_player_season <-
  memoise::memoise(
    function(url = "http://www.basketball-reference.com/leagues/NBA_1997_per_game.html",
             return_message = TRUE) {
      ## case_when

      page <-
        url %>%
        read_html()
      # Creates df that has columns: scheme (http), hostname(www.basketball-reference.com), and path(leagues/NBA_1997_per_game.htm)
      url_df <- url %>% httr::parse_url() %>% flatten_df()
      # Returns "1997_per_game"
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
      # Character Vector of Player Names
      players <-
        page %>%
        html_nodes('th+ .left a') %>%
        html_text()
      # Gets Player ID's Ex. "a/abdulma02.html"
      player_id <-
        page %>%
        html_nodes('th+ .left a') %>%
        html_attr('href') %>%
        # "/players/a/abdulma02.html"
        str_replace_all('/players/', '')

      player_ids <-
        player_id %>%
        map_chr(function(x) {
          x %>%
            # Get Player ID's and Remove .html. Ex. "a/abdulma02"
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
  )

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

.resolve.players <-
  memoise::memoise(function(data, site = "bref") {

    data <-
      data %>%
      mutate(
        namePlayer = case_when(
          namePlayer %>% str_detect("Larry Nance") &
            yearSeason > 1993 ~ "Larry Nance Jr.",
          namePlayer %>% str_detect("Tim Hardaway") &
            yearSeason > 2005 ~ "Tim Hardaway Jr.",
          namePlayer %>% str_detect("Gary Payton") &
            yearSeason > 2010 ~ "Gary Payton II",
          namePlayer %>% str_detect("Glenn Robinson") &
            yearSeason > 2012 ~ "Glenn Robinson III",
          namePlayer %>% str_detect("Jaren Jackson Jr.") &
            yearSeason > 2017 ~ "Jaren Jackson",
          namePlayer %>% str_detect("Gary Trent") &
            yearSeason > 2017 ~ "Gary Trent Jr.",
          TRUE                      ~  namePlayer
        )
      )

    data_players <-
      data %>%
      dplyr::select(one_of(c(
        "namePlayer", "slugPlayerBREF", "yearSeason"
      ))) %>%
      distinct()



    #### FIX 2nd gen players -- Nance - Hardaway - Rice -Payton

    if (!"df_dict_nba_players" %>% exists()) {
      assign_nba_players()
    }

    name_site <-
      glue::glue("namePlayer{str_to_upper(site)}")

    df_nba_stats_players <-
      df_dict_nba_players %>%
      dplyr::select(
        namePlayer,
        yearSeasonFirst,
        idPlayerNBA = idPlayer,
        urlPlayerThumbnail,
        urlPlayerHeadshot,
        urlPlayerPhoto,
        urlPlayerStats,
        urlPlayerActionPhoto,
      ) %>%
      mutate(namePlayerNBA = namePlayer) %>%
      dplyr::select(namePlayerNBA, everything()) %>%
      group_by(idPlayerNBA, namePlayerNBA) %>%
      filter(yearSeasonFirst == max(yearSeasonFirst)) %>%
      ungroup()

    data_players <-
      data_players %>%
      left_join(df_nba_stats_players) %>%
      suppressMessages()

    data_players <-
      data_players %>%
      mutate(
        idPlayerNBA = as.integer(idPlayerNBA),
        idPlayerNBA = case_when(
          slugPlayerBREF == "jonesma02" ~ 90000L,
          slugPlayerBREF == "jonesma03" ~ 2891L,
          slugPlayerBREF == "mitchto02" ~ 203502L,
          slugPlayerBREF == "mitchto03" ~ 203183L,
          slugPlayerBREF == "jonesbo01" ~ 77193L,
          slugPlayerBREF == "jonesbo02" ~ 200784L,
          slugPlayerBREF == "hendece01" ~ 76990L,
          slugPlayerBREF == "hendece02" ~ 1538L,
          slugPlayerBREF == "jonesch01" ~ 279L,
          slugPlayerBREF == "jonesch02" ~ 77178L,
          slugPlayerBREF == "jonesch03" ~ 1869L,
          slugPlayerBREF == "smithch01" ~ 293L,
          slugPlayerBREF == "smithch02" ~ 78179L,
          slugPlayerBREF == "smithch04" ~ 1520L,
          slugPlayerBREF == "johnsch03" ~ 202419L,
          slugPlayerBREF == "johnsch04" ~ 203187L,
          slugPlayerBREF == "smithch03" ~ 1814L,
          slugPlayerBREF == "smithch05" ~ 203147L,
          slugPlayerBREF == "brownde01" ~ 244L,
          slugPlayerBREF == "brownde03" ~ 200793L,
          slugPlayerBREF == "johnsed02" ~ 77144L,
          slugPlayerBREF == "johnsed03" ~ 698L,
          slugPlayerBREF == "johnsge02" ~ 77149L,
          slugPlayerBREF == "johnsge03" ~ 77148L,
          slugPlayerBREF == "kingge03" ~ 1628994L,
          slugPlayerBREF == "hendege01" ~ 76993L,
          slugPlayerBREF == "hendege02" ~ 201945L,
          slugPlayerBREF == "smithgr02" ~ 202962L,
          slugPlayerBREF == "paxsoji02" ~ 77819L,
          slugPlayerBREF == "johnske01" ~ 77154L,
          slugPlayerBREF == "johnske03" ~ 2256L,
          slugPlayerBREF == "johnsla02" ~ 913L,
          slugPlayerBREF == "willima03" ~ 200766L,
          slugPlayerBREF == "willima04" ~ 201173L,
          slugPlayerBREF == "davisma01" ~ 76528L,
          slugPlayerBREF == "davisma02" ~ 707L,
          slugPlayerBREF == "smithmi01" ~ 78197L,
          slugPlayerBREF == "smithmi02" ~ 63L,
          slugPlayerBREF == "dunlemi01" ~ 76616L,
          slugPlayerBREF == "dunlemi02" ~ 2399L,
          slugPlayerBREF == "jamesmi01" ~ 2229L,
          slugPlayerBREF == "jamesmi02" ~ 1628455L,
          slugPlayerBREF == "ewingpa01" ~ 121L,
          slugPlayerBREF == "ewingpa02" ~ 201607L,
          slugPlayerBREF == "willire01" ~ 199L,
          slugPlayerBREF == "willire02" ~ 202130L,
          slugPlayerBREF == "smithst03" ~ 200848L,
          slugPlayerBREF == "russewa01" ~ 78048L,
          slugPlayerBREF == "russewa02" ~ 201041L,
          slugPlayerBREF == "wrighch01" ~ 202874L,
          slugPlayerBREF == "wrighch02" ~ 203203L,
          slugPlayerBREF == "wilsobu01" ~ 78587L,
          slugPlayerBREF == "hawkiro01" ~ 76975L,
          slugPlayerBREF == "johnsch01" ~ 77133L,
          slugPlayerBREF == "johnsch02" ~ 77159L,
          slugPlayerBREF == "houseda01" ~ 1627863L,
          slugPlayerBREF == "brittda01" ~ 76261L,
          slugPlayerBREF == "willidu01" ~ 78545L,
          slugPlayerBREF == "lawreed01" ~ 77348L,
          slugPlayerBREF == "heardga01" ~ 76985L,
          slugPlayerBREF == "gilesha01" ~ 1628385L,
          slugPlayerBREF == "whitnha02" ~ 78519L,
          slugPlayerBREF == "vanbrja01" ~ 78400L,
          slugPlayerBREF == "whitejo01" ~ 78510L,
          slugPlayerBREF == "walkelo01" ~ 1629022L,
          slugPlayerBREF == "dampilo01" ~ 76499L,
          slugPlayerBREF == "baglema01" ~ 1628963L,
          slugPlayerBREF == "frazime01" ~ 1628982L,
          slugPlayerBREF == "creekmi01" ~ 1628249L,
          slugPlayerBREF == "bambamo01" ~ 1628964L,
          slugPlayerBREF == "williro04" ~ 1629057L,
          slugPlayerBREF == "valenro01" ~ 78396L,
          slugPlayerBREF == "hamilro01" ~ 76928L,
          slugPlayerBREF == "smithsa02" ~ 78205L,
          slugPlayerBREF == "mykhasv01" ~ 1629004L,
          slugPlayerBREF == "mcclate01" ~ 77510L,
          slugPlayerBREF == "greento01" ~ 76879L,
          slugPlayerBREF == "browntr01" ~ 1628972L,
          slugPlayerBREF == "edwarvi01" ~ 1629053L,
          slugPlayerBREF == "lemonwa01" ~ 1627215L,
          slugPlayerBREF == "cartewe01" ~ 1628976L,
          slugPlayerBREF == "davisch01" ~ 76519L,
          slugPlayerBREF == "duffybo02" ~ 76610L,
          slugPlayerBREF == "guokama02" ~ 76908L,
          slugPlayerBREF == "johnsge01" ~ 77147L,
          slugPlayerBREF == "johnsla01" ~ 77156L,
          slugPlayerBREF == "joneswi01" ~ 77202L,
          slugPlayerBREF == "kingge01" ~ 77268L,
          slugPlayerBREF == "paxsoji01" ~ 77818L,
          slugPlayerBREF == "smithbi01" ~ 78209L,
          slugPlayerBREF == "smithgr01" ~ 78209L,
          slugPlayerBREF == "turneja01" ~ 78380L,
          slugPlayerBREF == "turneja02" ~ 78382L,
          slugPlayerBREF == "ackerdo01" ~ 76008L,
          slugPlayerBREF == "actonbu01" ~ 76010L,
          slugPlayerBREF == "anderda02" ~ 76036L,
          slugPlayerBREF == "armstcu01" ~ 76060L,
          slugPlayerBREF == "athadi01" ~ 76069L,
          slugPlayerBREF == "attleal01" ~ 76070L,
          slugPlayerBREF == "austijo01" ~ 76073L,
          slugPlayerBREF == "averibi01" ~ 76076L,
          slugPlayerBREF == "barrmo01" ~ 76113L,
          slugPlayerBREF == "bellwh01" ~ 76143L,
          slugPlayerBREF == "bockhbu01" ~ 76191L,
          slugPlayerBREF == "bonsage01" ~ 76203L,
          slugPlayerBREF == "borsaik01" ~ 76209L,
          slugPlayerBREF == "boydfr01" ~ 76222L,
          slugPlayerBREF == "brianfr01" ~ 76250L,
          slugPlayerBREF == "bryanem01" ~ 76289L,
          slugPlayerBREF == "budkowa01" ~ 76298L,
          slugPlayerBREF == "burdeti01" ~ 76303L,
          slugPlayerBREF == "clemeba01" ~ 76403L,
          slugPlayerBREF == "crossch01" ~ 76479L,
          slugPlayerBREF == "darcepe01" ~ 76506L,
          slugPlayerBREF == "davisdw01" ~ 76521L,
          slugPlayerBREF == "davismi02"  ~ 76522L,
          slugPlayerBREF == "davisre01" ~ 76524L,
          slugPlayerBREF ==  "devlico01" ~ 76561L,
          slugPlayerBREF ==  "dilloho01" ~ 76571L,
          slugPlayerBREF ==  "doveso01" ~ 76592L,
          slugPlayerBREF ==  "eddledi01" ~ 76636L,
          slugPlayerBREF ==  "eichhdi01" ~ 76651L,
          slugPlayerBREF ==  "ellisbo01" ~ 76658L,
          slugPlayerBREF ==  "gaborbi01" ~ 76768L,
          slugPlayerBREF ==  "gaineel01" ~ 76769L,
          slugPlayerBREF ==  "garredi01" ~ 76788L,
          slugPlayerBREF ==  "gibsoho01" ~ 76811L,
          slugPlayerBREF ==  "glickno01" ~ 76823L,
          slugPlayerBREF ==  "hahnro01" ~ 76914L,
          slugPlayerBREF ==  "halbech01" ~ 76918L,
          slugPlayerBREF ==  "halbrsw01" ~ 76919L,
          slugPlayerBREF ==   "hermskl01" ~ 77007L,
          slugPlayerBREF ==   "hertzso01" ~ 77011L,
          slugPlayerBREF ==   "hoffmpa01" ~ 77036L,
          slugPlayerBREF ==   "holtaw01" ~ 77046L,
          slugPlayerBREF ==   "hoskebi01" ~ 77061L,
          slugPlayerBREF ==   "hundlho01" ~ 77082L,
          slugPlayerBREF ==   "ingramc01" ~ 77095L,
          slugPlayerBREF ==   "johnsra01" ~ 77139L,
          slugPlayerBREF ==  "joneswa01" ~ 77199L,
          slugPlayerBREF ==   "kearnmi01" ~ 77230L,
          slugPlayerBREF ==   "kennego01" ~ 77243L,
          slugPlayerBREF ==   "kennepi01" ~ 77245L,
          slugPlayerBREF ==   "kenvibi01" ~ 77247L,
          slugPlayerBREF ==   "kerrre01" ~ 77248L,
          slugPlayerBREF ==   "killuea01" ~ 77257L,
          slugPlayerBREF ==   "kingma01" ~ 77272L,
          slugPlayerBREF ==   "kronto01" ~ 77313L,
          slugPlayerBREF ==   "kupeccj01" ~ 77325L,
          slugPlayerBREF ==   "laytomo01" ~ 77350L,
          slugPlayerBREF ==  "leaksma01" ~ 77351L,
          slugPlayerBREF ==   "leeru01" ~ 77365L,
          slugPlayerBREF ==   "leonasl01" ~ 77371L,
          slugPlayerBREF ==   "loganjo01" ~ 77403L,
          slugPlayerBREF ==   "lowerch01" ~ 77416L,
          slugPlayerBREF ==   "mahonmo01" ~ 77444L,
          slugPlayerBREF ==  "marshri01" ~ 77465L,
          slugPlayerBREF ==  "martila01" ~ 77475L,
          slugPlayerBREF ==  "martiwh01" ~ 77479L,
          slugPlayerBREF ==  "mcconbu01" ~ 77514L,
          slugPlayerBREF ==  "mcguial01" ~ 77536L,
          slugPlayerBREF ==  "mckinbo01" ~ 77546L,
          slugPlayerBREF ==  "mclemmc01" ~ 77548L,
          slugPlayerBREF == "mcmulma01" ~ 77556L,
          slugPlayerBREF ==   "meinemo01" ~ 77573L,
          slugPlayerBREF ==   "morrire01" ~ 77648L,
          slugPlayerBREF == "nashco01" ~ 77687L,
          slugPlayerBREF == "niemiri01" ~ 77717L,
          slugPlayerBREF == "nordmbe01" ~ 77727L,
          slugPlayerBREF == "obriera01" ~ 77743L,
          slugPlayerBREF == "ogdenbu01" ~ 77747L,
          slugPlayerBREF == "oldhajo01" ~ 77758L,
          slugPlayerBREF == "olsenbu01" ~ 77765L,
          slugPlayerBREF == "owensre01" ~ 77783L,
          slugPlayerBREF == "parhaea01" ~ 77797L,
          slugPlayerBREF == "parkme01" ~ 77799L,
          slugPlayerBREF == "patteto01" ~ 77813L,
          slugPlayerBREF == "phelaja02" ~ 77849L,
          slugPlayerBREF == "rackllu01" ~ 77894L,
          slugPlayerBREF == "reisech01" ~ 77938L,
          slugPlayerBREF == "rittete01" ~ 77968L,
          slugPlayerBREF == "sandeto01" ~ 78060L,
          slugPlayerBREF == "saulpe01" ~ 78063L,
          slugPlayerBREF == "scolafr01" ~ 78094L,
          slugPlayerBREF == "searske01" ~ 78106L,
          slugPlayerBREF == "sharech01" ~ 78125L,
          slugPlayerBREF == "shippch01" ~ 78653L,
          slugPlayerBREF == "skinnta01" ~ 78168L,
          slugPlayerBREF == "smithwi01" ~ 78207L,
          slugPlayerBREF == "sobekch01" ~ 78214L,
          slugPlayerBREF == "stallbu01" ~ 78239L,
          slugPlayerBREF == "taylofa01" ~ 78302L,
          slugPlayerBREF == "todormi01" ~ 78346L,
          slugPlayerBREF == "towerbl01" ~ 78363L,
          slugPlayerBREF == "vanbrbu01" ~ 78401L,
          slugPlayerBREF == "vaughch01" ~ 78409L,
          slugPlayerBREF == "walthis01" ~ 78448L,
          slugPlayerBREF == "warrejo01" ~ 1969L,
          slugPlayerBREF == "whitask01" ~ 78506L,
          slugPlayerBREF == "williar01" ~ 78539L,
          slugPlayerBREF == "zawolze01" ~ 78639L,
          TRUE ~ as.integer(idPlayerNBA)
        )
      ) %>%
      select(yearSeasonFirst, namePlayer:idPlayerNBA) %>%
      distinct()

    remove_names <- df_nba_stats_players %>%
      select(-c(
        namePlayer, idPlayerNBA
      )) %>% names()

    data_players <-
      data_players %>%
      select(-one_of(remove_names)) %>%
      left_join(
        df_nba_stats_players %>% select(-namePlayer)
      ) %>%
      suppressMessages() %>%
      suppressWarnings()


    has_double <-
      data_players %>% select(one_of(c(
        "namePlayer", "namePlayerNBA", "slugPlayerBREF", "idPlayerNBA"
      ))) %>% distinct() %>% count(slugPlayerBREF) %>%
      filter(n > 1) %>% nrow() > 0

    if (has_double) {
      data_players <-
        data_players %>%
        left_join(data_players %>% select(one_of(
          c("namePlayer", "namePlayerNBA", "idPlayer", "idPlayerNBA")
        )) %>% distinct() %>% count(idPlayer)) %>%
        suppressMessages()

      df_unique <-
        data_players %>%
        filter(n == 1) %>%
        dplyr::select(-n)

      df_double <-
        data_players %>%
        dplyr::filter(n > 1) %>%
        dplyr::select(namePlayer, idPlayer, yearSeason) %>%
        distinct() %>%
        group_by(namePlayer, idPlayer) %>%
        transmute(yearSeasonFirst = min(yearSeason, na.rm = T)) %>%
        ungroup()


      df_double_2 <-
        data_players %>%
        filter(n > 1) %>%
        select(namePlayer, idPlayer, yearSeason) %>%
        distinct()


      df_double <-
        df_double %>%
        left_join(df_double_2) %>%
        left_join(df_nba_stats_players) %>%
        suppressMessages()

      data_players <-
        df_unique %>%
        bind_rows(df_double) %>%
        arrange(namePlayer)
    }

    df_na_players <-
      data_players %>%
      filter(is.na(namePlayerNBA))

    if (df_na_players %>% nrow() == 0) {
      data <-
        data %>%
        left_join(data_players) %>%
        suppressMessages()
      return(data)
    }

    df_good <-
      data_players %>%
      filter(!is.na(namePlayerNBA))

    players_good <- df_good %>% pull(namePlayer)

    df_good <-
      df_good %>%
      mutate(!!name_site := players_good) %>%
      dplyr::select(dplyr::matches("idPlayer|namePlayer"), everything())

    df_good <-
      df_good %>%
      left_join(data) %>%
      dplyr::select(-namePlayerNBA) %>%
      suppressMessages()

    df_formulas <-
      .dictionary.sites()

    df_player_dict <-
      df_formulas %>%
      filter(nameSite == site %>% str_to_lower()) %>%
      pull(formulatDict) %>%
      rlang::parse_expr() %>%
      eval()

    players_na <-
      df_na_players %>% pull(namePlayer)

    df_na_players <-
      df_na_players %>%
      select(namePlayer) %>%
      distinct() %>%
      left_join(df_player_dict) %>%
      mutate(!!name_site := unique(players_na)) %>%
      left_join(data) %>%
      dplyr::select(-namePlayer) %>%
      dplyr::rename(namePlayer = namePlayerNBA) %>%
      left_join(
        df_dict_nba_players %>% select(namePlayer, idPlayerNBA = idPlayer, urlPlayerThumbnail)
      ) %>%
      suppressMessages()

    df <-
      df_good %>%
      bind_rows(df_na_players) %>%
      distinct() %>%
      dplyr::select(-one_of("namePlayerNBA")) %>%
      suppressWarnings() %>%
      filter(!is.na(idPlayerNBA)) %>%
      arrange(namePlayer)

    df


  })

.dictionary.sites <-
  memoise::memoise(function() {
    df_formulas <-
      tibble(
        nameSite = c("bref", "yahoo", "realgm", "hoopshype"),
        formulatDict = c(
          ".dictionary.bref.nba.missing()",
          "dictionary.yahoo.nba.missing()",
          "dictionary.realgm.nba.missing()",
          "dictionary.hoopshype.nba.missing()"
        )
      )
    df_formulas
  })

.dictionary.bref.nba.missing <-
  memoise::memoise(function() {
    tibble(
      namePlayer = c(
        "J.J. Anderson",
        "Tiny Archibald",
        "Billy Ray Bates",
        "Chubby Cox",
        "Geoff Crompton",
        "Charles Davis",
        "World B. Free",
        "Dave Greenwood",
        "Joe Hassett",
        "Hutch Jones",
        "Fat Lever",
        "Charles Pittman",
        "James Ray",
        "Cliff Robinson",
        "Danny Schayes",
        "Ed Sherod",
        "Pete Verhoeven",
        "DeWayne Scales",
        "Michael Wilson",
        "Kenton Edelin",
        "Mike Holton",
        "Melvin Turpin",
        "Eddie Lee Wilkins",
        "Michael Phelps",
        "Maurice Martin",
        "McKinley Singleton",
        "Pearl Washington",
        "Ron Grandison",
        "Rob Lock",
        "Rob Rose",
        "Isaac Austin",
        "Steve Bardo",
        "LaBradford Smith",
        "Steve Smith",
        "Jo Jo English",
        "Clarence Weatherspoon",
        "Rich Manning",
        "Logan Vander Velden",
        "Horacio Llamas Grey",
        "LaMark Baker",
        "Makhtar N'Diaye",
        "Jeffrey Sheppard",
        "Stanislav Medvedenko",
        "Mamadou N'Diaye",
        "Wang Zhizhi",
        "Isaac Fontaine",
        "Norm Richardson",
        "Nene Hilario",
        "Roger Mason",
        "Ronald Murray",
        "Efthimi Rentzias",
        "Mike Sweetney",
        "Didier Ilunga-Mbenga",
        "Ibo Kutluay",
        "Ha Seung-Jin",
        "J.R. Smith",
        "C.J. Miles",
        "Boniface N'Dong",
        "J.J. Redick",
        "P.J. Tucker",
        "D.J. Strawberry",
        "J.J. Hickson",
        "D.J. White",
        "Sun Yue",
        "A.J. Price",
        "Eugene Jeter",
        "Hamady N'Diaye",
        "Perry Jones",
        "Luigi Datome",
        "C.J. McCollum",
        "Otto Porter",
        "D.J. Stephens",
        "James Ennis",
        "P.J. Hairston",
        "K.J. McDaniels",
        "Johnny O'Bryant",
        "T.J. Warren",
        "C.J. Wilcox",
        "R.J. Hunter",
        "J.J. O'Brien",
        "Kelly Oubre",
        "Wade Baldwin",
        "A.J. Hammons",
        "Derrick Jones",
        "Taurean Waller-Prince",
        "Wesley Iwundu",
        "T.J. Leaf",
        "Zhou Qi",
        "Dennis Smith",
        "Derrick Walton",
        "Andrew White",
        "James Webb",
        "Juan Hernangomez", "Marcus Williams",
        "Matt Williams", "Mike Dunleavy", "Mike James", "Vince Hunter",
        "Walt Lemon, Jr."
      )
      ,
      namePlayerNBA = c(
        "Mitchell Anderson",
        "Nate Archibald",
        "Billyray Bates",
        "John Cox",
        "Jeffrey Crompton",
        "Charlie Davis",
        "World Free",
        "David Greenwood",
        "Joey Hassett",
        "Willie Jones",
        "Lafayette Lever",
        "Charlie Pittman",
        "Jamesearl Ray",
        "Cliff T. Robinson",
        "Dan Schayes",
        "Edmund Sherod",
        "Peter Verhoeven",
        "Dewayne Scales",
        "Mike Wilson",
        "Kent Edelin",
        "Michael Holton",
        "Mel Turpin",
        "Eddielee Wilkins",
        "Mike Phelps",
        "Mo Martin",
        "Mckinley Singleton",
        "Dwayne Washington",
        "Ronnie Grandison",
        "Robert Lock",
        "Robert Rose",
        "Ike Austin",
        "Stephen Bardo",
        "Labradford Smith",
        "Steven Smith",
        "Jojo English",
        "Clar. Weatherspoon",
        "Richard Manning",
        "Log Vander Velden",
        "Horacio Llamas",
        "Mark Baker",
        "Makhtar N'diaye",
        "Jeff Sheppard",
        "Slava Medvedenko",
        "Mamadou N'diaye",
        "Wang Zhi-zhi",
        "Ike Fontaine",
        "Norman Richardson",
        "Nene",
        "Roger Mason Jr.",
        "Flip Murray",
        "Efthimios Rentzias",
        "Michael Sweetney",
        "DJ Mbenga",
        "Ibrahim Kutluay",
        "Ha Seung-Jin",
        "JR Smith",
        "CJ Miles",
        "Boniface N'Dong",
        "JJ Redick",
        "PJ Tucker",
        "DJ Strawberry",
        "JJ Hickson",
        "DJ White",
        "Sun Yue",
        "AJ Price",
        "Pooh Jeter",
        "Hamady Ndiaye",
        "Perry Jones III",
        "Gigi Datome",
        "CJ McCollum",
        "Otto Porter Jr.",
        "DJ Stephens",
        "James Ennis III",
        "PJ Hairston",
        "KJ McDaniels",
        "Johnny O'Bryant III",
        "TJ Warren",
        "CJ Wilcox",
        "RJ Hunter",
        "JJ O'Brien",
        "Kelly Oubre Jr.",
        "Wade Baldwin IV",
        "AJ Hammons",
        "Derrick Jones Jr.",
        "Taurean Prince",
        "Wes Iwundu",
        "TJ Leaf",
        "Qi Qi",
        "Dennis Smith Jr",
        "Derrick Walton Jr.",
        "Andrew White III",
        "Juancho Hernangomez",
        "James Webb III",
        "Marcus Williams",
        "Matt Williams Jr.", "Mike Dunleavy Jr.", "Mike James", "Vincent Hunter",
        "Walter Lemon Jr."
      )
    )

  })

.dictionary_player_tables <-
  memoise::memoise(function() {
    tibble(
      nameTable = c(
        "passes",
        "clutch",
        "game splits",
        "general splits",
        "opponent",
        "next n games",
        "player on off details",
        "defense",
        "game logs",
        "rebounding",
        "shot chart detail",
        "shots",
        "year over year",
        "fantasy profile"

      ),
      slugTable = c(
        "playerdashptpass",
        "playerdashboardbyclutch",
        "playerdashboardbygamesplits",
        "playerdashboardbygeneralsplits",
        "playerdashboardbyopponent",
        "playernextngames",
        "leagueplayerondetails",
        "playerdashptshotdefend",
        "playergamelogs",
        "playerdashptreb",
        "shotchartdetail",
        "playerdashptshots",
        "playerdashboardbyyearoveryear",
        "playerfantasyprofile"
      )
    )
  })








# ---- NBA.com ----
#' NBA players table data
#'
#' @param players vector of player names
#' @param player_ids vector of player ids
#' @param tables vector of tables options \itemize{
#' \item passes
#' \item clutch
#' \item game splits
#' \item general splits
#' \item opponent
#' \item next n games
#' \item player on off details
#' \item defense
#' \item game logs
#' \item rebounding
#' \item shot chart detail
#' \item shots
#' \item year over year
#' \item fantasy profile
#' }
#' @param measures vector of measure types options include \itemize{
#' \item Base
#' \item Advanced
#' \item Misc
#' \item Scoring
#' \item Four Factors
#' \item Opponent
#' \item Usage
#' \item Defense
#' }
#' @param seasons vector of seasons
#' @param modes vector of modes options include \itemize{
#' \item PerGame
#' \item Totals
#' \item MinutesPer
#' \item Per48
#' \item Per40
#' \item Per36
#' \item PerMinute
#' \item PerPossession
#' \item PerPlay
#' \item Per100Possessions
#' \item Per100Plays
#' }#'
#' @param playoff_rounds vector of playoff rounds options include code{0:4}
#' @param is_plus_minus \code{TRUE} returns plus minus
#' @param is_rank if \code{TRUE} returns rank
#' @param is_pace_adjusted if \code{TRUE} adjusts for pace
#' @param outcomes vector of outcomes options include \itemize{
#' \item NA
#' \item Wins
#' \item Losses
#' }
#' @param locations vector of locations options include \itemize{
#' \item NA
#' \item Home
#' \item Road
#' }
#' @param months vector of game months options include \code{0:12}
#' @param season_segments vector of season segments, options include \itemize{
#' \item NA
#' \item Post All-Star
#' \item Pre All-Star
#' }
#' @param date_from \code{NA} or date from
#' @param date_to \code{NA} or date to
#' @param opponent_ids vector of opponent ids
#' @param vs_confs vector of conferences against options include  \itemize{
#' \item NA
#' \item East
#' \item West
#' }
#' @param vs_divisions vector of divisions against options include \itemize{
#' \item NA
#' \item Atlantic
#' \item Central
#' \item Northwest
#' \item Pacific
#' \item Southeast
#' \item Southwest
#' }
#' @param game_segments vector of game segments options include \itemize{
#' \item NA
#' \item First Half
#' \item Second Half
#' \item Overtime
#' }
#' @param periods vector of periods \code{0:12}
#' @param last_n_games vector of last_n games \code{0:82}
#' @param season_types vector of season types options include \itemize{
#' \item Regular Season
#' \item Pre Season
#' \item Playoffs
#' \item All Star
#' }
#' @param n_games number n of last games
#' @param shot_clocks  vector of shot clock ranges options include \itemize{
#' \item  NA,
#' \item 24-22
#' \item 22-18 Very Early
#' \item 18-15 Early
#' \item 15-7 Average
#' \item 7-4 Late
#' \item 4-0 Very Late
#' \item ShotClock Off
#' }
#' @param return_message if \code{TRUE} returns a message
#' @param assign_to_environment if \code{TRUE} assigns data to environment
#'
#' @return a \code{tibble}
#'
#' @export
#' @family player
#' @examples
#' players_tables(players = c("Caris LeVert", "Joe Harris"), tables =  c("year over year", "passes", "game splits"),   modes = c("PerGame", "Totals"), measures = c("Base", "Advanced"), assign_to_environment = TRUE)
players_tables <-
  function(players = NULL,
           player_ids = NULL,
           tables = c("year over year", "passes", "game splits"),
           measures = "Base",
           seasons = 2019,
           modes = c("PerGame", "Totals"),
           season_types = "Regular Season",
           playoff_rounds = NA,
           is_plus_minus = F,
           n_games = 20,
           is_rank = F,
           is_pace_adjusted = F,
           outcomes = NA,
           locations = NA,
           months = NA,
           season_segments = NA,
           date_from = NA,
           date_to = NA,
           opponent_ids = NA,
           vs_confs = NA,
           vs_divisions = NA,
           game_segments = NA,
           periods = NA,
           shot_clocks =  NA,
           last_n_games = NA,
           assign_to_environment = TRUE,
           return_message = TRUE) {

    assign_nba_players()

    ids <-
      nba_player_ids(player_ids = player_ids,
                     players = players)

    input_df <-
      expand.grid(
        player_id = ids,
        table = tables,
        measure = measures,
        season = seasons,
        mode = modes,
        n_game = n_games,
        season_type = season_types,
        playoff_round = playoff_rounds,
        is_plus_minus = is_plus_minus,
        is_rank = is_rank,
        is_pace_adjusted = is_pace_adjusted,
        outcome =  outcomes,
        location = locations,
        month  = months,
        season_segment = season_segments,
        date_from = date_from,
        date_to = date_to,
        opponent_id = opponent_ids,
        vs_conf = vs_confs,
        vs_division = vs_divisions,
        game_segment = game_segments,
        period = periods,
        shot_clock = shot_clocks,
        last_n_games = last_n_games,
        stringsAsFactors = F
      ) %>%
      dplyr::as_tibble()
    get_player_table_data_safe <-
      purrr::possibly(.get_player_table_data, tibble())

    all_data <-
      1:nrow(input_df) %>%
      future_map_dfr(function(x) {
        df_row <-
          input_df %>% slice(x)
        df_row %$%
          get_player_table_data_safe(
            player_id = player_id,
            table = table,
            measure = measure,
            season = season,
            mode = mode,
            season_type = season_type,
            game_id = NA,
            context_measure = NA,
            playoff_round = playoff_round,
            is_plus_minus = is_plus_minus,
            is_rank = is_rank,
            is_pace_adjusted = is_pace_adjusted,
            outcome = outcome,
            location = location,
            month = month,
            season_segment = season_segment,
            date_from = date_from,
            date_to = date_to,
            opponent_id = opponent_id,
            vs_conf = vs_conf,
            vs_division = vs_division,
            game_segment = game_segment,
            period = period,
            shot_clock = shot_clock,
            last_n_games = last_n_games,
            return_message = return_message
          )
      })
    df_dict_table_names <-
      .dictionary_player_tables()

    table_names <-
      df_dict_table_names$nameTable %>% map_chr(function(x) {
        generate_data_name(x = x, result = "Player")
      })

    df_dict_table_names <-
      df_dict_table_names %>%
      mutate(tableName = table_names) %>%
      select(-nameTable) %>%
      dplyr::rename(tableSlugName = tableName)

    all_data <-
      all_data %>%
      left_join(df_dict_nba_players %>% select(idPlayer, dplyr::matches("url"))) %>%
      suppressWarnings() %>%
      suppressMessages()

    all_data <-
      all_data %>%
      left_join(df_dict_table_names) %>%
      select(tableSlugName, nameTable, everything()) %>%
      suppressMessages() %>%
      unique()

    if (assign_to_environment) {
      all_tables <-
        all_data$tableSlugName %>%
        unique()
      all_tables %>%
        walk(function(table) {
          df_tables <-
            all_data %>%
            filter(tableSlugName == table) %>%
            select(-one_of(c("slugTable", "tableSlugName"))) %>%
            unnest() %>%
            remove_na_columns()


          has_measure <- df_tables %>% tibble::has_name("typeMeasure")

          if (has_measure) {
            measures <-
              df_tables$typeMeasure %>% unique()
            measures %>%
              walk(function(measure) {
                table_name <-
                  table %>%
                  str_c(measure)
                df_table <-
                  df_tables %>%
                  filter(typeMeasure == measure) %>%
                  unnest() %>%
                  remove_na_columns() %>%
                  distinct()
                assign(x = table_name,
                       value = df_table,
                       envir = .GlobalEnv)
              })
          } else{
            df_table <-
              df_tables %>%
              unnest() %>%
              remove_na_columns() %>%
              distinct()

            if (df_table %>% tibble::has_name("idPlayer1")){
              data <- data %>% select(-idPlayer1)
            }

            assign(x = table,
                   value = df_table,
                   envir = .GlobalEnv)
          }
        })
    }
    all_data %>%
      remove_na_columns()
  }

#' Players' NBA player ids
#'
#' Resolve player ids for specified players
#'
#' @param players vector of player names
#' @param player_ids vector of player ids
#'
#' @return a `tibble`
#' @export
#' @import dplyr stringr jsonlite readr purrr tibble tidyr curl
#' @family ids
#' @examples
#' nba_player_ids(players = c("Mitch Richmond", "Kyle Kuzma"))
nba_player_ids <-
  function(players = NULL, player_ids = NULL) {

    if (player_ids %>% purrr::is_null() && players %>% purrr::is_null()) {
      stop("Please enter players of player ids")
    }

    ids <- c()

    if (!player_ids %>% purrr::is_null()) {
      ids <-
        ids %>%
        append(player_ids)
    }

    if (!'df_nba_player_dict' %>% exists()) {
      df_nba_player_dict <-
        nba_players()

      assign(x = 'df_nba_player_dict', df_nba_player_dict, envir = .GlobalEnv)
    }

    if (!players %>% purrr::is_null() ) {
      player_search <-
        players %>% str_c(collapse = "|")

      search_ids <-
        df_nba_player_dict %>%
        filter(namePlayer %>% str_detect(player_search)) %>%
        pull(idPlayer) %>%
        unique()

      ids <-
        ids %>%
        append(search_ids)

    }
    ids %>% unique() %>% sort()
  }

.get_player_table_data <-
  function(player_id = 1627747,
           table = "year over year",
           measure = "Base",
           season = 2018,
           mode = "PerGame",
           season_type = "Regular Season",
           game_id = NA,
           n_game = NA,
           context_measure = "FGM",
           playoff_round = NA,
           is_plus_minus = F,
           is_rank = F,
           is_pace_adjusted = F,
           outcome = NA,
           location = NA,
           month = NA,
           season_segment = NA,
           date_from = NA,
           date_to = NA,
           opponent_id = NA,
           vs_conf = NA,
           vs_division = NA,
           game_segment = NA,
           period = NA,
           shot_clock =  NA,
           last_n_games = NA,
           return_message = TRUE) {
    df_player_slug_tables <-
      .dictionary_player_tables()
    assign_nba_players()

    player <-
      df_dict_nba_players %>%
      filter(idPlayer == player_id) %>%
      pull(namePlayer)

    if (return_message) {
      glue::glue("Acquiring {player} {season} {season_type} {measure} {table} {mode} data") %>% cat(fill = T)
    }

    table_slug <-
      df_player_slug_tables %>%
      filter(nameTable  == (str_to_lower(table))) %>%
      pull(slugTable)
    URL <- gen_url(table_slug)
    measure_slug <-
      generate_call_slug(x = str_to_title(measure), default_value = "Base")
    mode_slug <-
      generate_call_slug(x = mode, default_value = "PerGame")
    context_measure_slug = generate_call_slug(x = context_measure, default_value = "")
    season_slug <- generate_season_slug(season = season)
    game_id_slug <-
      generate_call_slug(x = game_id, default_value = 0)
    season_type_slug  = generate_call_slug(x = season_type, default_value = "Regular+Season")
    playoff_round_slug = generate_call_slug(x = playoff_round, default_value = 0)
    plus_minus_slug <-
      generate_call_slug(x = is_plus_minus , default_value = "N")
    rank_slug <-
      generate_call_slug(x = is_rank , default_value = "N")
    pace_slug <-
      generate_call_slug(x = is_pace_adjusted , default_value = "N")
    outcome_slug <-
      generate_call_slug(x = outcome , default_value = "")
    location_slug <-
      generate_call_slug(x = location , default_value = "")
    month_slug <- generate_call_slug(x = month , default_value = 0)
    season_segment_slug <-
      generate_call_slug(x = season_segment , default_value = "")
    date_from_slug <-
      generate_call_slug(x = date_from , default_value = "")
    date_to_slug <-
      generate_call_slug(x = date_to , default_value = "")
    opponent_id_slug <-
      generate_call_slug(x = opponent_id , default_value = 0)
    vs_conf_slug <-
      generate_call_slug(x = season_segment , default_value = "")
    vs_division_slug <-
      generate_call_slug(x = vs_division , default_value = "")
    game_segment_slug  <-
      generate_call_slug(x = game_segment , default_value = "")
    period_slug <-
      generate_call_slug(x = period , default_value = 0)
    shot_clock_slug <-
      generate_call_slug(x = shot_clock , default_value = "")
    last_n_games_slug <-
      generate_call_slug(x = last_n_games , default_value = 0)
    game_number_slug <-
      generate_call_slug(x = n_game , default_value = 0)
    params <-
      list(
        measureType = measure_slug,
        perMode = mode_slug,
        plusMinus = plus_minus_slug,
        contextMeasure = context_measure_slug,
        paceAdjust = pace_slug,
        rank = rank_slug,
        leagueId = "00",
        season = season_slug,
        seasonType = season_type,
        GameID = game_id_slug,
        TeamID = 0,
        GROUP_ID = 0,
        numberOfGames = game_number_slug,
        poRound = playoff_round_slug,
        playerID = player_id,
        outcome = outcome_slug,
        location = location_slug,
        month = month_slug,
        seasonSegment = season_segment_slug,
        dateFrom = date_from_slug,
        dateTo = date_to_slug,
        opponentTeamId = opponent_id_slug,
        vsConference = vs_conf_slug,
        vsDivision = vs_division_slug,
        gameSegment = game_segment_slug,
        period = period_slug,
        shotClockRange = shot_clock_slug,
        lastNGames = last_n_games_slug
      )
    if (table_slug == "teamvsplayer") {
      names(params)[names(params) %>% str_detect("teamId")] <-
        "playerId"
    }
    slug_param <-
      .generate_param_slug(params = params)

    url <-
      glue::glue("{URL}?{slug_param}") %>% as.character()

    resp <-
      url %>%
      curl() %>%
      readr::read_lines()

    json <-
      resp %>% jsonlite::fromJSON(simplifyVector = T)
    all_data <-
      .parse_player_json(
        json = json,
        player = player,
        season = season,
        mode = mode,
        measure = measure,
        season_type = season_type
      ) %>%
      mutate(idPlayer = player_id,
             typeMeasure = measure,
             modeSearch = mode,
             slugSeason = season_slug,
             yearSeason = season)  %>%
      dplyr::select(one_of(c("nameTable", "typeMeasure", "modeSearch", "slugSeason", "yearSeason",
                             "typeSeason", "slugSeasonSearch",
                             "idPlayer", "namePlayer", "slugTable",
                             "dataTable")
      ), everything()) %>%
      suppressWarnings()
    all_data
  }

generate_data_name <- function(x, result = "Team") {
  x %>%
    str_split("\\ ") %>%
    flatten_chr() %>%
    str_to_title() %>%
    str_c(collapse   = "") %>%
    str_c("data", ., str_to_title(result))
}

.parse_player_json <- function(json, player = player, season = season, mode, measure, season_type) {
  table_length <-
    json$resultSets$rowSet %>% length()
  table_slug <- json$resource

  season_slug <-
    generate_season_slug(season = season)

  all_data <-
    1:table_length %>%
    future_map_dfr(function(x) {
      table_name <-
        json$resultSets$name[x]

      df_parameters <- json$parameters %>% flatten_df()

      df_parameters <-
        df_parameters %>%
        purrr::set_names(names(df_parameters) %>% resolve_nba_names()) %>%
        munge_nba_data()
      df_parameters <-
        df_parameters %>%
        mutate_at(df_parameters %>% dplyr::select(dplyr::matches("is[A-Z]")) %>% names(),
                  funs(ifelse(. == "Y", 1, 0) %>% as.logical())) %>%
        mutate(numberTable = x,) %>%
        select(one_of(c("numberTable", "typeMeasure", "modeSearch")), everything()) %>%
        suppressWarnings()

      json_names <-
        json$resultSets$headers[[x]]

      actual_names <-
        json_names %>%
        resolve_nba_names()

      data <-
        json$resultSets$rowSet[[x]] %>%
        data.frame(stringsAsFactors = F) %>%
        dplyr::as_tibble()

      if (data %>% nrow() == 0) {
        return(invisible())
      }

      data <-
        data  %>%
        purrr::set_names(actual_names) %>%
        munge_nba_data() %>%
        mutate(numberTable = x,
               namePlayer = player,
               slugSeason = season_slug,
               typeMeasure = measure,
               modeSearch = mode,
               typeSeason = season_type) %>%
        dplyr::select(one_of(
          c(
            "typeSeason",
            "modeSearch",
            "typeMeasure",
            "numberTable",
            "slugSeason",
            "namePlayer"
          )
        ), everything()) %>%
        suppressWarnings()

      if (table_name == "NextNGames") {
        data <-
          data %>%
          unite(nameTeamHome, cityTeamHome, teamNameHome,  sep =  " ") %>%
          unite(nameTeamAway, cityTeamAway, teamNameAway,  sep =  " ") %>%
          mutate(dateGame = dateGame %>% lubridate::mdy())
      }

      if (data %>% tibble::has_name("typeShot")){
        data <-
          data %>%
          dplyr::rename(typeFilter = typeShot)
      }

      if (table_name == "PlayersSeasonTotals"){
        if (data %>% has_name("namePlayer")) {
          data <-
            data %>%
            dplyr::rename(typeFilter = namePlayer)
        }

      }

      if (table_name == "TeamOverall"){
        data <-
          data %>%
          mutate(nameGroup = "Players")

      }
      if (table_name == "OverallPlayerDashboard") {
        return(invisible())
      }

      if (table_name %in% c("OverallPlayerDashboard", "ByYearPlayerDashboard")){
        data <-
          data %>%
          dplyr::rename(slugSeasonSearch = slugSeason,
                        slugSeason = typeFilter) %>%
          mutate(modeSearch = mode,
                 typeMeasure = measure,
                 namePlayer = player)

      }

      data <-
        data %>%
        left_join(df_parameters) %>%
        dplyr::select(one_of(names(df_parameters)), everything()) %>%
        suppressMessages() %>%
        select(-numberTable) %>%
        mutate(nameTable = table_name) %>%
        select(nameTable, everything()) %>%
        dplyr::select(-one_of("idLeague")) %>%
        remove_zero_sum_cols() %>%
        mutate(slugTable = table_slug,
               yearSeason = season) %>%
        suppressWarnings() %>%
        suppressMessages()

      data <-
        data %>%
        dplyr::select(nameTable:slugSeason, yearSeason, everything())

      data <-
        data %>%
        dplyr::select(-one_of(c( "resultGame", "locationGame",
                                 "nameSeasonMonth", "segmentSeason", "rangeDaysRest"))) %>%
        suppressWarnings()

      if (table_name == "ByYearTeamDashboard") {
        if (data %>% tibble::has_name("slugSeason")) {
          data <-
            data %>%
            dplyr::rename(slugSeasonSearch = slugSeason)
        }

        if (data %>% tibble::has_name("groupByYear")) {
          data <-
            data %>%
            dplyr::rename(slugSeason = groupByYear)
        }
      }


      if (table_name %in% c("OverallTeamDashboard", "OverallTeamPlayerOnOffSummary")) {
        return(invisible())
      }

      if (table_name == "AssistedBy") {
        assign_nba_players()
        data <-
          data %>%
          dplyr::rename(idPlayerAssistedBy = idPlayer) %>%
          dplyr::select(-one_of("namePlayer")) %>%
          left_join(
            df_dict_nba_players %>% select(idPlayerAssistedBy = idPlayer,
                                           namePlayerAssistedBy = namePlayer)
          ) %>%
          suppressMessages()

        data <-
          data %>%
          dplyr::select(dplyr::matches("type|mode|^is|^id|^name"),
                        everything())
      }

      key_cols <-
        c("slugTable", "nameTable","typeSeason", "slugSeason", "yearSeason", "slugSeasonSearch", "namePlayer",
          "typeMeasure", "modeSearch") %>% unique()

      nest_cols <-
        names(data)[!names(data) %in% key_cols]

      data <-
        data %>%
        nest_(key_col = 'dataTable', nest_cols = nest_cols)
      data
    })

  all_data

}
