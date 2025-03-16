
get_following <- function(tweet_id){
  
  page <- 1
  full <- list()
  next_page_token <- ""
  
  repeat {
    cat("Grab Page:", page, "\n")   # keep track of pages (not necessary but could be useful)
    
    params <- list(`user.fields` = 'created_at',
                   `expansions` = 'pinned_tweet_id')
    
    if (next_page_token == "")
    {url_handle <- 
      glue::glue("https://api.twitter.com/2/users/{status_id}/following?max_results=1000", 
                 status_id = tweet_id)}
    else
    {url_handle <- glue::glue("https://api.twitter.com/2/users/{status_id}/following?max_results=1000&pagination_token={page_token}", status_id = tweet_id, page_token = next_page_token)}
    
    response <- httr::GET(url = url_handle,
                          httr::add_headers(.headers = headers))
    
    obj <- httr::content(response, as = "text")
    
    x <- rjson::fromJSON(obj)
    full <- c(full, x$data %>% purrr::map_chr("id"))
    
    if (is.null(x$meta$next_token))
    {
      cat("Final Page Counts Stops at:", page, "\n")
      break
    }
    next_page_token <-x$meta$next_token
    page = page + 1
  }
  return(unlist(full))
}