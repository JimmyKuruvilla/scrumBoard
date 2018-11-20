rm(list=ls())

source(here::here("functions", "endDate.R"))
source(here::here("functions", "endDateActual.R"))
source(here::here("functions", "countIssues.R"))
source(here::here("functions", "countIssuesOpen.R"))
source(here::here("functions", "countIssuesState.R"))
source(here::here("functions", "plotBurn.R"))
source(here::here("functions", "plotBarRepo.R"))
source(here::here("functions", "plotBarStory.R"))
source(here::here("functions", "plotLineType.R"))
source(here::here("functions", "plotBarMEpics.R"))
source(here::here("functions", "plotBarMGantt.R"))
source(here::here("functions", "plotBarMBacklog.R"))

save.image(here::here("data", "plots.RData"))

rm(list=ls())