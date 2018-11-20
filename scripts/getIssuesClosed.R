rm(list=ls())

source(here::here("constants.R"))
source(here::here("functions", "getRepos.R"))
source(here::here("functions", "getMilestones.R"))
source(here::here("functions", "getIssues.R"))
source(here::here("functions", "getIssuesByMilestone.R"))
source(here::here("functions", "parseHeaderLink.R"))
source(here::here("functions", "parseRepos.R"))
source(here::here("functions", "parseMilestones.R"))
source(here::here("functions", "parseIssues.R"))

repos <- getRepos(token = env$token)
repos <- parseRepos(reposList = repos)
repos <- subset(repos, repos$owner=="RMSone")
repos <- rbind(subset(repos, grepl(pattern = "miu", x = repos$name)),
               subset(repos, grepl(pattern = "swagger", x = repos$name)))

milestones <- lapply(X = repos$full_name, FUN = getMilestones, token = env$token)
milestones <- parseMilestones(milestonesList = milestones)
milestonesClosed <- subset(milestones, milestones$state=="closed")
# milestonesClosed <- subset(milestones, milestones$title=="Contracts v0.4")

issuesClosed_new <- mapply(FUN = getIssuesByMilestone, repo = milestonesClosed$repo, milestone = milestonesClosed$number, MoreArgs = list(token = env$token), SIMPLIFY = F, USE.NAMES = F)
issuesClosed_new <- parseIssues(issuesList = issuesClosed_new)
issuesClosed_new <- subset(issuesClosed_new, issuesClosed_new$url!="none")
issuesClosed_new <- subset(issuesClosed_new, issuesClosed_new$pull_request==0)
issuesClosed <- rbind(issuesClosed_new)
# issuesClosed <- rbind(issuesClosed, issuesClosed_new)

rm(list=setdiff(ls(), "issuesClosed"))
save.image(here::here("data", "issuesClosed.RData"))

rm(list=ls())

