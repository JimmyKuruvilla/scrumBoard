library(httr)

getMilestones <- function(repo, token){
  url <- paste0("https://api.github.com/repos/",repo,"/milestones?state=all")
  get <- GET(url = url, add_headers(Authorization = token), verbose())
  content <- content(get)
  return(content)
}