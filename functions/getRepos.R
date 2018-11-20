library(httr)

getRepos <- function(token){
  url <- paste0("https://api.github.com/user/repos")
  get <- GET(url = url, add_headers(Authorization = token))
  headers <- headers(get)
  headerLinks <- headers$link
  if(is.null(headerLinks)){
    content <- content(get)
    return(content)
  } else {
    content <- content(get)
    headerLinks <- parseHeaderLink(headerLinks)
    nextLink <- subset(headerLinks, rel=="next",select = "url")
    while(nrow(nextLink)>0){
      nextUrl <- as.character(nextLink)
      nextGet <- GET(url = nextUrl, add_headers(Authorization = token))
      nextContent <- content(nextGet)
      content <- c(content, nextContent)
      nextHeaders <- headers(nextGet)
      nextHeaderLinks <- nextHeaders$link
      nextHeaderLinks <- parseHeaderLink(nextHeaderLinks)
      nextLink <- subset(nextHeaderLinks, rel=="next",select = "url")
    }
    return(content)
  }
}