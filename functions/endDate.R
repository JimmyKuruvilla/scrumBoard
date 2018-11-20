endDate <- function(issuesDf, ms){
  sub <- subset(issuesDf, issuesDf$milestone.title==ms)
  sub <- subset(sub, sub$repo!="RMSone/miu-planning")
  end <- as.Date(unique(sub$milestone.due_on))
  return(end)
}