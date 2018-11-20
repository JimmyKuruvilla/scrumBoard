parseRepos <- function(reposList){
  df <- unlist(reposList)
  df <- data.frame(full_name = df[names(df)=="full_name"],
                   name = df[names(df)=="name"],
                   owner = df[names(df)=="owner.login"],
                   stringsAsFactors = F)
  return(df)
}