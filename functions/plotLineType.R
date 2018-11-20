library(ggplot2)

# ## for troubleshooting
# rm(list=ls())
# load("issuesOpen.RData")
# load("issuesClosed.RData")
# issues <- rbind(issuesClosed, issuesOpen)
# issuesDf <- issues
# ms <- "Miu v18"

plotLineType <- function(issuesDf, ms){
  sub <- subset(issuesDf, issuesDf$milestone.title==ms)
  sub <- subset(sub, sub$repo!="RMSone/miu-planning")
  
  subTask <- sub
  subEnhance <- subset(sub, sub$label.name=="enhancement" | sub$label.name=="code-enhancement")
  subBug <- subset(sub, sub$label.name=="bug")
  
  subTask <- unique(subTask[,c("url", "repo", "number", "created_at", "closed_at")])
  subEnhance <- unique(subEnhance[,c("url", "repo", "number", "created_at", "closed_at", "label.name")])
  subBug <- unique(subBug[,c("url", "repo", "number", "created_at", "closed_at", "label.name")])
  
  subTask$label.name <- "task"
  
  subEnhance <- subEnhance
  subBug <- subset(subBug, !(subBug$url %in% c(subEnhance$url)))
  subTask <- subset(subTask, !(subTask$url %in% c(subEnhance$url, subBug$url)))
  
  df <- rbind(subEnhance, subBug, subTask)
  df_c <- subset(df, df$closed_at!="none")
  
  start <- as.Date(unique(sub$milestone.description))
  startGraph <- start - 24
  end <- as.Date(max(df_c$closed_at))
  endTarget <- as.Date(unique(sub$milestone.due_on))
  endGraph <- if(!is.na(end)){max(end, endTarget)}else{endTarget}
  dates <- seq(from = startGraph, to = endGraph, by = 1)
  d <- length(dates)
  
  countOpenIssues <- function(i){
    date_i <- dates[[i]]
    sub_i <- subset(df, df$created_at<=date_i)
    n <- nrow(sub_i)
    bugs_i <- nrow(subset(sub_i, sub_i$label.name=="bug"))
    enhance_i <- nrow(subset(sub_i, sub_i$label.name=="enhancement" | sub_i$label.name=="code-enhancement"))
    tasks_i <- nrow(subset(sub_i, sub_i$label.name=="task"))
    return(list(date = date_i, tasks = tasks_i, bugs = bugs_i, enhancements = enhance_i))
  }
  
  countClosedIssues <- function(i){
    date_i <- dates[[i]]
    sub_i <- subset(df_c, df_c$closed_at<=date_i)
    n <- nrow(sub_i)
    bugs_i <- nrow(subset(sub_i, sub_i$label.name=="bug"))
    enhance_i <- nrow(subset(sub_i, sub_i$label.name=="enhancement" | sub_i$label.name=="code-enhancement"))
    tasks_i <- nrow(subset(sub_i, sub_i$label.name=="task"))
    return(list(date = date_i, tasks = tasks_i, bugs = bugs_i, enhancements = enhance_i))
  }
  
  dfOpen <- unlist(lapply(X = 1:d, FUN = countOpenIssues), use.names = T)
  dfOpenDate <- dfOpen[names(dfOpen)=="date"]
  dfOpenTasks <- data.frame(countOpen = dfOpen[names(dfOpen)=="tasks"], class = "tasks")
  dfOpenBugs <- data.frame(countOpen = dfOpen[names(dfOpen)=="bugs"], class = "bugs")
  dfOpenEnhance <- data.frame(countOpen = dfOpen[names(dfOpen)=="enhancements"], class = "enhancements")
  dfOpen <- rbind(dfOpenTasks, dfOpenBugs, dfOpenEnhance)
  dfOpen$date <- as.Date(rep(dfOpenDate, times = 3), origin = "1970-01-01")
  
  dfClosed <- unlist(lapply(X = 1:d, FUN = countClosedIssues), use.names = T)
  dfClosedDate <- dfClosed[names(dfClosed)=="date"]
  dfClosedTasks <- data.frame(countClosed = dfClosed[names(dfClosed)=="tasks"], class = "tasks")
  dfClosedBugs <- data.frame(countClosed = dfClosed[names(dfClosed)=="bugs"], class = "bugs")
  dfClosedEnhance <- data.frame(countClosed = dfClosed[names(dfClosed)=="enhancements"], class = "enhancements")
  dfClosed <- rbind(dfClosedTasks, dfClosedBugs, dfClosedEnhance)
  dfClosed$date <- as.Date(rep(dfClosedDate, times = 3), origin = "1970-01-01")
  
  df <- merge(x = dfOpen, dfClosed, by = c("date", "class"), all = T)
  df$countNet <- df$countOpen - df$countClosed
  
  plot <- ggplot(data = df)
  plot <- plot + aes(x = date, y = countNet, fill = class)
  plot <- plot + geom_area(position = 'stack')
  plot <- plot + scale_fill_manual(values = c("#9E9E9E", "#F44336", "#4CAF50"))
  plot <- plot + theme(legend.position="right")
  plot <- plot + theme(panel.background = element_rect(fill = 'white'))
  plot <- plot + theme(axis.text=element_text(size=12))
  plot <- plot + theme(legend.text=element_text(size=12))
  plot <- plot + theme(axis.title=element_blank())
  plot <- plot + theme(legend.title=element_blank())
  plot <- plot + xlim(startGraph, endGraph)
  plot <- plot + geom_vline(aes(xintercept=start),linetype=2,size=1,colour="black")
  plot
}
