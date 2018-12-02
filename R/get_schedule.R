#' Gets schedule data and box score URLs for specified league and season
#' 
#' Returns a data frame of league schedule and game data for user supplied leagues & seasons.
#' 
#' @param league Leagues from which the user wants to scrape data ("OHL" and/or "WHL" and/or "QMJHL").
#' @param season Seasons for which the user wants to scrape data. Must be of the form \code{2017-18}, \code{1995-96}, etc.
#' @param progress Sets a Progress Bar. Defaults to \code{TRUE}.
#' @param ... Allows the user to supply other information to the function. If you don't know what this means, then don't worry about it.
#' @examples 
#' get_schedule("OHL", "2018-19")
#' 
#' get_schedule("QMJHL", "1999-00", progress = FALSE)
#' 
#' get_schedule(c("OHL", "WHL", "QMJHL"), c("2018-19", "2017-18"))
#' 
#' @export
#' @import dplyr
#' 
get_schedule <- function(league, season, ..., progress = TRUE) {
  
  leagues <- league %>%
    as_tibble() %>%
    purrr::set_names("league") %>%
    mutate_all(toupper) %>%
    distinct()
  
  seasons <- season %>%
    as_tibble() %>%
    purrr::set_names("season") %>%
    distinct()
  
  mydata <- tidyr::crossing(leagues, seasons)
  
  if (progress) {
    
    pb <- progress::progress_bar$new(format = "get_schedule() [:bar] :percent eta: :eta", clear = FALSE, total = nrow(mydata), show_after = 0) 
    
    pb$tick(0)
    
  }
  
  driver <- RSelenium::rsDriver(verbose = FALSE)
  
  on.exit(driver$client$close())
  on.exit(driver$server$stop())
  
  .get_schedule <- function(league, season, ...) {
    
    if (league == "OHL") {  
      
      if (season == "2018-19") {url = "http://ontariohockeyleague.com/schedule/63"}
      
      else if (season == "2017-18") {url = "http://ontariohockeyleague.com/schedule/60"}
      else if (season == "2016-17") {url = "http://ontariohockeyleague.com/schedule/56"}
      else if (season == "2015-16") {url = "http://ontariohockeyleague.com/schedule/54"}
      else if (season == "2014-15") {url = "http://ontariohockeyleague.com/schedule/51"}
      else if (season == "2013-14") {url = "http://ontariohockeyleague.com/schedule/49"}
      else if (season == "2012-13") {url = "http://ontariohockeyleague.com/schedule/46"}
      else if (season == "2011-12") {url = "http://ontariohockeyleague.com/schedule/44"}
      else if (season == "2010-11") {url = "http://ontariohockeyleague.com/schedule/42"}
      else if (season == "2009-10") {url = "http://ontariohockeyleague.com/schedule/38"}
      else if (season == "2008-09") {url = "http://ontariohockeyleague.com/schedule/35"}
      else if (season == "2007-08") {url = "http://ontariohockeyleague.com/schedule/32"}
      else if (season == "2006-07") {url = "http://ontariohockeyleague.com/schedule/29"}
      else if (season == "2005-06") {url = "http://ontariohockeyleague.com/schedule/26"}
      else if (season == "2004-05") {url = "http://ontariohockeyleague.com/schedule/24"}
      else if (season == "2003-04") {url = "http://ontariohockeyleague.com/schedule/21"}
      else if (season == "2002-03") {url = "http://ontariohockeyleague.com/schedule/17"}
      else if (season == "2001-02") {url = "http://ontariohockeyleague.com/schedule/14"}
      else if (season == "2000-01") {url = "http://ontariohockeyleague.com/schedule/11"}
      else if (season == "1999-00") {url = "http://ontariohockeyleague.com/schedule/9"}
      else if (season == "1998-99") {url = "http://ontariohockeyleague.com/schedule/6"}
      else if (season == "1997-98") {url = "http://ontariohockeyleague.com/schedule/4"}
      
      else {stop("Season not available. Sorry!")}
      
    }
    
    else if (league == "QMJHL") {
      
      if (season == "2018-19") {url = "http://theqmjhl.ca/schedule/190"}
      
      else if (season == "2017-18") {url = "http://theqmjhl.ca/schedule/187"}
      else if (season == "2016-17") {url = "http://theqmjhl.ca/schedule/184"}
      else if (season == "2015-16") {url = "http://theqmjhl.ca/schedule/181"}
      else if (season == "2014-15") {url = "http://theqmjhl.ca/schedule/178"}
      else if (season == "2013-14") {url = "http://theqmjhl.ca/schedule/175"}
      else if (season == "2012-13") {url = "http://theqmjhl.ca/schedule/171"}
      else if (season == "2011-12") {url = "http://theqmjhl.ca/schedule/168"}
      else if (season == "2010-11") {url = "http://theqmjhl.ca/schedule/82"}
      else if (season == "2009-10") {url = "http://theqmjhl.ca/schedule/164"}
      else if (season == "2008-09") {url = "http://theqmjhl.ca/schedule/162"}
      else if (season == "2007-08") {url = "http://theqmjhl.ca/schedule/160"}
      else if (season == "2006-07") {url = "http://theqmjhl.ca/schedule/104"}
      else if (season == "2005-06") {url = "http://theqmjhl.ca/schedule/90"}
      else if (season == "2004-05") {url = "http://theqmjhl.ca/schedule/88"}
      else if (season == "2003-04") {url = "http://theqmjhl.ca/schedule/86"}
      else if (season == "2002-03") {url = "http://theqmjhl.ca/schedule/84"}
      else if (season == "2001-02") {url = "http://theqmjhl.ca/schedule/158"}
      else if (season == "2000-01") {url = "http://theqmjhl.ca/schedule/156"}
      else if (season == "1999-00") {url = "http://theqmjhl.ca/schedule/154"}
      else if (season == "1998-99") {url = "http://theqmjhl.ca/schedule/152"}
      else if (season == "1997-98") {url = "http://theqmjhl.ca/schedule/151"}
      else if (season == "1996-97") {url = "http://theqmjhl.ca/schedule/148"}
      else if (season == "1995-96") {url = "http://theqmjhl.ca/schedule/146"}
      else if (season == "1994-95") {url = "http://theqmjhl.ca/schedule/144"}
      else if (season == "1993-94") {url = "http://theqmjhl.ca/schedule/142"}
      else if (season == "1992-93") {url = "http://theqmjhl.ca/schedule/140"}
      else if (season == "1991-92") {url = "http://theqmjhl.ca/schedule/138"}
      else if (season == "1990-91") {url = "http://theqmjhl.ca/schedule/136"}
      else if (season == "1989-90") {url = "http://theqmjhl.ca/schedule/134"}
      else if (season == "1988-89") {url = "http://theqmjhl.ca/schedule/132"}
      else if (season == "1987-88") {url = "http://theqmjhl.ca/schedule/130"}
      else if (season == "1986-87") {url = "http://theqmjhl.ca/schedule/128"}
      else if (season == "1985-86") {url = "http://theqmjhl.ca/schedule/126"}
      else if (season == "1984-85") {url = "http://theqmjhl.ca/schedule/124"}
      else if (season == "1983-84") {url = "http://theqmjhl.ca/schedule/122"}
      else if (season == "1982-83") {url = "http://theqmjhl.ca/schedule/120"}
      else if (season == "1981-82") {url = "http://theqmjhl.ca/schedule/118"}
      else if (season == "1980-81") {url = "http://theqmjhl.ca/schedule/116"}
      else if (season == "1979-80") {url = "http://theqmjhl.ca/schedule/114"}
      else if (season == "1978-79") {url = "http://theqmjhl.ca/schedule/112"}
      else if (season == "1977-78") {url = "http://theqmjhl.ca/schedule/110"}
      else if (season == "1976-77") {url = "http://theqmjhl.ca/schedule/108"}
      else if (season == "1975-76") {url = "http://theqmjhl.ca/schedule/106"}
      else if (season == "1974-75") {url = "http://theqmjhl.ca/schedule/102"}
      else if (season == "1973-74") {url = "http://theqmjhl.ca/schedule/100"}
      else if (season == "1972-73") {url = "http://theqmjhl.ca/schedule/98"}
      else if (season == "1971-72") {url = "http://theqmjhl.ca/schedule/95"}
      else if (season == "1970-71") {url = "http://theqmjhl.ca/schedule/93"}
      else if (season == "1969-70") {url = "http://theqmjhl.ca/schedule/91"}
      
      else {stop("Season not available. Sorry!")}
      
    }
    
    else if (league == "WHL") {
      
      if (season == "2018-19") {url = "http://whl.ca/schedule/266"}
      
      else if (season == "2017-18") {url = "http://whl.ca/schedule/262"}
      else if (season == "2016-17") {url = "http://whl.ca/schedule/257"}
      else if (season == "2015-16") {url = "http://whl.ca/schedule/251"}
      else if (season == "2014-15") {url = "http://whl.ca/schedule/249"}
      else if (season == "2013-14") {url = "http://whl.ca/schedule/245"}
      else if (season == "2012-13") {url = "http://whl.ca/schedule/242"}
      else if (season == "2011-12") {url = "http://whl.ca/schedule/238"}
      else if (season == "2010-11") {url = "http://whl.ca/schedule/236"}
      else if (season == "2009-10") {url = "http://whl.ca/schedule/234"}
      else if (season == "2008-09") {url = "http://whl.ca/schedule/231"}
      else if (season == "2007-08") {url = "http://whl.ca/schedule/229"}
      else if (season == "2006-07") {url = "http://whl.ca/schedule/227"}
      else if (season == "2005-06") {url = "http://whl.ca/schedule/225"}
      else if (season == "2004-05") {url = "http://whl.ca/schedule/223"}
      else if (season == "2003-04") {url = "http://whl.ca/schedule/220"}
      else if (season == "2002-03") {url = "http://whl.ca/schedule/217"}
      else if (season == "2001-02") {url = "http://whl.ca/schedule/215"}
      else if (season == "2000-01") {url = "http://whl.ca/schedule/213"}
      else if (season == "1999-00") {url = "http://whl.ca/schedule/211"}
      else if (season == "1998-99") {url = "http://whl.ca/schedule/209"}
      else if (season == "1997-98") {url = "http://whl.ca/schedule/206"}
      else if (season == "1996-97") {url = "http://whl.ca/schedule/204"}
      
      else {stop("Season not available. Sorry!")}
      
    }
    
    else {stop("League not available. Sorry!")}
    
    seq(2, 5, by = 0.001) %>%
      sample(1) %>%
      Sys.sleep()
    
    driver$client$navigate(url)
    
    Sys.sleep(3)
    
    page <- driver$client$getPageSource() %>%
      purrr::pluck(1) %>%
      xml2::read_html()
    
    url_prefix <- case_when(league == "OHL" ~ "http://ontariohockeyleague.com", 
                            league == "QMJHL" ~ "http://theqmjhl.ca",
                            league == "WHL" ~ "http://whl.ca")
    
    season_short <- season %>%
      stringr::str_split("\\-", simplify = TRUE, n = 2) %>%
      purrr::pluck(1) %>%
      as.numeric()
    
    game_urls <- page %>%
      rvest::html_nodes('[title="Game Centre"]') %>%
      rvest::html_attr("href") %>%
      stringr::str_c(url_prefix, .) %>%
      stringr::str_replace_all(c("play_by_play" = "boxscore")) %>%
      as_tibble() %>%
      purrr::set_names("url")
    
    if (league == "QMJHL") {
      
      date <- page %>%
        rvest::html_nodes(".table__td--schedule-date") %>%
        rvest::html_text() %>%
        as_tibble() %>%
        purrr::set_names("date")
      
    }
    
    else if (league %in% c("OHL", "WHL")) {
      
      date <- page %>%
        rvest::html_nodes(".table__td--schedule-date") %>%
        rvest::html_text() %>%
        as_tibble() %>%
        purrr::set_names("month_day") %>%
        mutate(month_day = stringr::str_split(month_day, ", ", simplify = TRUE, n = 2)[,2]) %>%
        mutate(month = stringr::str_split(month_day, " ", simplify = TRUE, n = 2)[,1]) %>%
        mutate(day = stringr::str_split(month_day, " ", simplify = TRUE, n = 2)[,2]) %>%
        mutate(year = ifelse(month %in% c("Aug", "Sep", "Oct", "Nov", "Dec"), season_short, season_short + 1)) %>%
        mutate(date = stringr::str_c(year, month, day, sep = "-")) %>%
        mutate(date = lubridate::ymd(date)) %>%
        mutate(date = as.character(date)) %>%
        select(date)
      
    }
    
    if (league == "OHL") {
      
      attendance <- as.character(NA)
      
    }
    
    else if (league == "WHL") {
      
      attendance <- page %>%
        rvest::html_nodes(".table__td--+ .table__td") %>%
        rvest::html_text() %>%
        as.character()
      
    }
    
    else if (league == "QMJHL") {
      
      attendance <- page %>%
        rvest::html_nodes(".table__td--schedule-time+ .table__td") %>%
        rvest::html_text() %>%
        as.character()
      
    }
    
    game_notes <- page %>%
      rvest::html_nodes(".table__td--schedule-time") %>%
      rvest::html_text() %>%
      as_tibble() %>%
      purrr::set_names("game_notes") %>%
      mutate(game_notes = stringr::str_replace_all(game_notes, c("Final" = "")))
    
    venue <- page %>%
      rvest::html_nodes(".table__td--") %>%
      rvest::html_text() %>%
      as_tibble() %>%
      purrr::set_names("venue")
    
    goals <- page %>%
      rvest::html_nodes(".table__td--schedule-score") %>%
      rvest::html_text() %>%
      as_tibble() %>%
      purrr::set_names("goals")
    
    away_goals <- goals %>%
      filter(row_number() %% 2 == 1) %>%
      purrr::set_names("away_goals")
    
    home_goals <- goals %>%
      filter(row_number() %% 2 == 0) %>%
      purrr::set_names("home_goals")
    
    home_team <- page %>%
      rvest::html_nodes(".table__td--schedule-home") %>%
      rvest::html_text() %>%
      as_tibble() %>%
      purrr::set_names("home_team") %>%
      mutate_all(toupper)
    
    away_team <- page %>%
      rvest::html_nodes(".table__td--schedule-away") %>%
      rvest::html_text() %>%
      as_tibble() %>%
      purrr::set_names("away_team") %>%
      mutate_all(toupper)
    
    schedule <- date %>%
      bind_cols(away_team) %>%
      bind_cols(away_goals) %>%
      bind_cols(home_team) %>%
      bind_cols(home_goals) %>%
      bind_cols(game_notes) %>%
      bind_cols(venue) %>%
      bind_cols(game_urls) %>%
      mutate(attendance = attendance) %>%
      mutate(league = league) %>%
      mutate(season = season) %>%
      mutate(date = stringr::str_extract(date, "[0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2}")) %>%
      filter(!stringr::str_detect(url, "preview")) %>%
      mutate(venue = ifelse(league == "WHL", stringr::str_split(venue, "\\-[^\\-]*$", simplify = TRUE, n = 2)[,1], venue)) %>%
      mutate_all(stringr::str_squish) %>%
      mutate_all(~na_if(., "")) %>%
      mutate_at(vars(away_goals, home_goals, attendance), as.numeric) %>%
      distinct() %>%
      select(league, season, date, away_team, away_goals, home_team, home_goals, game_notes, venue, attendance, url)
    
    if (progress) {pb$tick()}
    
    return(schedule)
    
  }
  
  schedule_data <- purrr::map2_dfr(mydata[["league"]], mydata[["season"]], .get_schedule)
  
  return(schedule_data)
  
}