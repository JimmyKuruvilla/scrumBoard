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
milestonesOpen <- subset(milestones, milestones$state=="open")

issuesOpen <- mapply(FUN = getIssuesByMilestone, repo = milestonesOpen$repo, milestone = milestonesOpen$number, MoreArgs = list(token = env$token), SIMPLIFY = F, USE.NAMES = F)
issuesOpen <- parseIssues(issuesList = issuesOpen)
issuesOpen <- subset(issuesOpen, issuesOpen$url!="none")
issuesOpen <- subset(issuesOpen, issuesOpen$pull_request==0)

rm(list=setdiff(ls(), c("issuesOpen", "milestones")))
save.image(here::here("data", "issuesOpen.RData"))

rm(list=ls())