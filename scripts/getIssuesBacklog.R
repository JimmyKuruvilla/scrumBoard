rm(list=ls())

source(here::here("constants.R"))
source(here::here("functions", "getIssuesByMilestone.R"))
source(here::here("functions", "parseHeaderLink.R"))
source(here::here("functions", "parseIssues.R"))

repo <- "RMSone/miu-planning"
milestone <- "none"

issuesBacklog <- mapply(FUN = getIssuesByMilestone, repo = repo, milestone = milestone, MoreArgs = list(token = env$token), SIMPLIFY = F, USE.NAMES = F)
issuesBacklog <- parseIssues(issuesList = issuesBacklog)
issuesBacklog <- subset(issuesBacklog, issuesBacklog$url!="none")
issuesBacklog <- subset(issuesBacklog, issuesBacklog$pull_request==0)

rm(list=setdiff(ls(), c("issuesBacklog")))
save.image(here::here("data", "issuesBacklog.RData"))

rm(list=ls())