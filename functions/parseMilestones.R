parseMilestones <- function(milestonesList){
  df <- unlist(milestonesList)
  df <- data.frame(url = df[names(df)=="url"],
                   title = df[names(df)=="title"],
                   number = df[names(df)=="number"],
                   due_on = df[names(df)=="due_on"],
                   description = df[names(df)=="description"],
                   state = df[names(df)=="state"],
                   open_issues = df[names(df)=="open_issues"],
                   closed_issues = df[names(df)=="closed_issues"],
                   stringsAsFactors = F)
  df$repo <- paste0(data.frame(matrix(unlist(strsplit(x = df$url, split = "/")), ncol = 8, byrow = T), stringsAsFactors = F)$X5,
                    "/",
                    data.frame(matrix(unlist(strsplit(x = df$url, split = "/")), ncol = 8, byrow = T), stringsAsFactors = F)$X6)
  return(df)
}