endDateActual <- function(issuesDf, ms){
  sub <- subset(issuesDf, issuesDf$milestone.title==ms)
  sub <- subset(sub, sub$repo!="RMSone/miu-planning")
  subO <- subset(sub, sub$closed_at=="none")
  end <- as.Date(unique(sub$milestone.due_on))
  endA <- if(nrow(subO)>0){end}else{as.Date(max(sub$closed_at))}
  return(endA)
}