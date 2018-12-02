#' Gets box score scoring summary data
#' 
#' Returns a data frame of the box score scoring summary (including goal, primary assist, and secondary assist) for specified URL. 
#' 
#' @param ... Function takes an object from \code{get_schedule()}. Function requires a \code{url}, \code{league}, and \code{season}. Additional data may be supplied. All of this information comes directly from \code{get_schedule()}.
#' @param progress Sets a Progress Bar. Defaults to \code{TRUE}.
#' @examples 
#' ## Use get_schedule() to get the games and URLs desired
#' games <- get_schedule("OHL", "2018-19")
#' 
#' ## Easy peasy get box score scoring summary data
#' get_box_score(games)
#' 
#' ## If you want to see 1 game, try using dplyr::slice to only select the 1st game
#' games %>% 
#'   slice(1) %>% 
#'   get_box_score()
#' 
#' @export
#' @import dplyr
#' 
get_box_score <- function(..., progress = TRUE) {
  
  if (progress) {
    
    pb <- progress::progress_bar$new(format = "get_box_score() [:bar] :percent eta: :eta", clear = FALSE, total = nrow(...), show_after = 0) 
    
    pb$tick(0)}
  
  driver <- RSelenium::rsDriver(verbose = FALSE)
  
  on.exit(driver$client$close())
  on.exit(driver$server$stop())
  
  .get_box_score <- function(url, league, season, ...) {
    
    seq(2, 5, by = 0.001) %>%
      sample(1) %>%
      Sys.sleep()
    
    driver$client$navigate(url)
    
    Sys.sleep(3)
    
    page <- driver$client$getPageSource() %>%
      purrr::pluck(1) %>%
      xml2::read_html()
    
    league_alternative_name <- case_when(league == "OHL" ~ "ohl", 
                                         league == "WHL" ~ "whl", 
                                         league == "QMJHL" ~ "lhjmq")
    
    teams <- page %>% 
      rvest::html_nodes(".gamecentre-playbyplay-event--goal") %>%
      {tibble(team = as(., "character"))} %>%
      mutate(team = stringr::str_split(team, stringr::str_c('<div class="gamecentre-playbyplay-event team-border--', league_alternative_name, '-', sep = ""), simplify = TRUE, n = 2)[,2]) %>%
      mutate(team = stringr::str_split(team, 'gamecentre-playbyplay-event--goal', simplify = TRUE, n = 2)[,1]) %>%
      mutate(team = toupper(team))
    
    goal_info <- page %>%
      rvest::html_nodes(".gamecentre-playbyplay-event--goal") %>%
      rvest::html_text() %>%
      as_tibble() %>%
      purrr::set_names("messy_data") %>%
      mutate(period = stringr::str_split(messy_data, " ", simplify = TRUE, n = 2)[,1]) %>%
      mutate(period = stringr::str_split(period, "Goal", simplify = TRUE, n = 2)[,2]) %>%
      mutate(period = stringr::str_replace_all(period, c("ST" = "", "ND" = "", "RD" = ""))) %>%
      mutate(time = stringr::str_split(messy_data, " ", simplify = TRUE, n = 2)[,2]) %>%
      mutate(time = stringr::str_split(time, "\\#", simplify = TRUE, n = 2)[,1]) %>%
      mutate(goal = stringr::str_split(messy_data, " ", simplify = TRUE, n = 3)[,3]) %>%
      mutate(goal = stringr::str_split(goal, "\\(", simplify = TRUE, n = 2)[,1]) %>%
      mutate(assists = stringr::str_split(messy_data, "Assists\\:", simplify = TRUE, n = 2)[,2]) %>%
      mutate(assists = stringr::str_split(assists, "\\+/-", simplify = TRUE, n = 2)[,1]) %>%
      mutate(game_strength = case_when(str_detect(messy_data, "Short Handed") & stringr::str_detect(messy_data, "Empty Net") ~ "SH EN",
                                       stringr::str_detect(messy_data, "Power Play") & stringr::str_detect(messy_data, "Empty Net") ~ "PP EN",
                                       stringr::str_detect(messy_data, "Short Handed") & stringr::str_detect(messy_data, "Penalty Shot") ~ "SH PS",
                                       stringr::str_detect(messy_data, "Power Play") & stringr::str_detect(messy_data, "Penalty Shot") ~ "PP PS",
                                       stringr::str_detect(messy_data, "Empty Net") ~ "EN",
                                       stringr::str_detect(messy_data, "Short Handed") ~ "SH",
                                       stringr::str_detect(messy_data, "Power Play") ~ "PP",
                                       stringr::str_detect(messy_data, "Penalty Shot") ~ "PS",
                                       TRUE ~ "EV")) %>%
      mutate(assists = stringr::str_replace_all(assists, c("Power Play" = "", 
                                                  "Short Handed" = "", 
                                                  "Empty Net" = "", 
                                                  "Penalty Shot" = "",
                                                  "Game Winning" = "", 
                                                  "Game Tying" = "",
                                                  "Insurance Goal" = ""))) %>%
      mutate(primary_assist = stringr::str_split(assists, ",", simplify = TRUE, n = 2)[,1]) %>%
      mutate(primary_assist = stringr::str_replace_all(primary_assist, "\\#[0-9]{1,2}", "")) %>%
      mutate(secondary_assist = stringr::str_split(assists, ",", simplify = TRUE, n = 2)[,2]) %>%
      mutate(secondary_assist = stringr::str_replace_all(secondary_assist, "\\#[0-9]{1,2}", ""))
    
    
    box_score_data <- teams %>%
      bind_cols(goal_info) %>%
      mutate(season = season) %>%
      mutate(league = league) %>%
      mutate(game_url = url) %>%
      select(time, period, game_strength, team, goal, primary_assist, secondary_assist, season, league, game_url) %>%
      mutate_all(str_squish) %>%
      mutate_all(~na_if(., ""))
    
    if (progress) {pb$tick()}
    
    return(box_score_data)
    
  }
  
  persistently_get_box_score <- chl::persistently(.get_box_score, max_attempts = 10, wait_seconds = 0.0001)
  
  try_get_box_score <- function(url, league, season, ...) {
    
    tryCatch(persistently_get_box_score(url, league, season, ...), 
             
             error = function(e) {
               print(e) 
               print(url)
               data_frame()},
             
             warning = function(w) {
               print(w) 
               print(url)
               data_frame()})
  }
  
  
  all_box_score_data <- purrr::pmap_dfr(..., try_get_box_score)
  
  return(all_box_score_data)
  
}