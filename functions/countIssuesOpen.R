countIssuesOpen <- function(issuesDf, ms){
  sub <- subset(issuesDf, issuesDf$milestone.title==ms)
  sub <- subset(sub, sub$repo!="RMSone/miu-planning")
  subU <- unique(sub[, c("repo", "number", "closed_at")])
  subO <- subset(subU, subU$closed_at=="none")
  count <- nrow(subO)
  return(count)
}