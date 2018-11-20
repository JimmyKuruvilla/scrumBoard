library(ggplot2)

# ## for troubleshooting
# rm(list=ls())
# load("issuesOpen.RData")
# load("issuesClosed.RData")
# issues <- rbind(issuesClosed, issuesOpen)
# issuesDf <- issues
# ms <- "Contracts v1.0"
# state <- "On PProd"

countIssuesState <- function(issuesDf, ms, state){
  sub <- subset(issuesDf, issuesDf$milestone.title==ms)
  sub <- subset(sub, sub$repo!="RMSone/miu-planning")
  
  subProd <- subset(sub, sub$closed_at!="none" | sub$label.name %in% c("deployed"))
  subPreProd <- subset(sub, sub$closed_at!="none" | sub$label.name %in% c("deployed", "on preprod", "deploy to prod"))
  subDev <- subset(sub, sub$closed_at!="none" | sub$label.name %in% c("deployed", "on preprod", "deploy to prod", "on upgrade", "on c_upgrade", "on dev", "testing", "deploy to preprod"))
  
  sub <- unique(sub[, c("repo", "number", "prefix")])
  subProd <- unique(subProd[, c("repo", "number", "prefix")])
  subPreProd <- unique(subPreProd[, c("repo", "number", "prefix")])
  subDev <- unique(subDev[, c("repo", "number", "prefix")])
  
  count <- as.data.frame(table(sub[, "prefix"]), stringsAsFactors = F)
  countProd <- as.data.frame(table(subProd[, "prefix"]), stringsAsFactors = F)
  countPreProd <- as.data.frame(table(subPreProd[, "prefix"]), stringsAsFactors = F)
  countDev <- as.data.frame(table(subDev[, "prefix"]), stringsAsFactors = F)
  
  colnames(count) <- c("prefix", "countTotal")
  
  if(nrow(countProd)!=0){colnames(countProd) <- c("prefix", "countProd")}else{countProd <- data.frame(prefix = count$prefix, countProd = rep(0, nrow(count)), stringsAsFactors = F)}
  if(nrow(countPreProd)!=0){colnames(countPreProd) <- c("prefix", "countPreProd")}else{countPreProd <- data.frame(prefix = count$prefix, countPreProd = rep(0, nrow(count)), stringsAsFactors = F)}
  if(nrow(countDev)!=0){colnames(countDev) <- c("prefix", "countDev")}else{countDev <- data.frame(prefix = count$prefix, countDev = rep(0, nrow(count)), stringsAsFactors = F)}
  
  stories <- subset(issuesDf, issuesDf$milestone.title==ms & issuesDf$repo=="RMSone/miu-planning" & issuesDf$label.name=="story")
  stories <- unique(stories[, c("prefix", "title")])
  
  epics <- subset(issuesDf, issuesDf$milestone.title==ms & issuesDf$repo=="RMSone/miu-planning" & issuesDf$label.name=="epic")
  epics$epic <- sapply(X = strsplit(epics$title, ": "), FUN = function(x){x[2]})
  epics$number <- sapply(X = strsplit(epics$prefix, "-"), FUN = function(x){x[2]})
  epics <- unique(epics[, c("number", "epic")])
  
  df <- merge(x = count, y = countProd, by = "prefix", all = T)
  df <- merge(x = df, y = countPreProd, by = "prefix", all = T)
  df <- merge(x = df, y = countDev, by = "prefix", all = T)
  
  df$to_do <- ifelse(is.na(df$countTotal), 0,df$countTotal) - ifelse(is.na(df$countDev), 0,df$countDev)
  df$on_dev <- ifelse(is.na(df$countDev), 0,df$countDev) - ifelse(is.na(df$countPreProd), 0,df$countPreProd)
  df$on_preprod <- ifelse(is.na(df$countPreProd), 0,df$countPreProd) - ifelse(is.na(df$countProd), 0,df$countProd)
  df$on_prod <- ifelse(is.na(df$countProd), 0,df$countProd)
  
  df <- rbind(data.frame(env = "To Do", prefix = df$prefix, count = df$to_do, stringsAsFactors = F),
              data.frame(env = "On Dev", prefix = df$prefix, count = df$on_dev, stringsAsFactors = F),
              data.frame(env = "On PProd", prefix = df$prefix, count = df$on_preprod, stringsAsFactors = F),
              data.frame(env = "On Prod", prefix = df$prefix, count = df$on_prod, stringsAsFactors = F))
  
  df$type <- sapply(X = strsplit(df$prefix, "-"), FUN = function(x){x[1]})
  df <- subset(df, df$type!="E")
  count <- sum(subset(df, df$env==state)$count)
  count
}
