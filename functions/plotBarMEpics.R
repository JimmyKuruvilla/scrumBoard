library(ggplot2)
library(plyr)
library(dplyr)
library(stringr)

# ## for troubleshooting
# rm(list=ls())
# load("issuesOpen.RData")
# load("issuesClosed.RData")
# issues <- rbind(issuesClosed, issuesOpen)
# issuesDf <- issues
# milestonesDf <- milestones
# ms <- "Contracts v0.1"

plotBarMEpics <- function(issuesDf, milestonesDf, ms){
  m <- unique(milestonesDf[, c("title", "description")])
  m$description <- as.Date(m$description)
  d <- subset(m, m$title==ms)$description
  m <- subset(m, m$description>=d)
  
  issuesDf <- subset(issuesDf, issuesDf$milestone.title %in% m$title)
  issuesDf <- subset(issuesDf, issuesDf$repo=="RMSone/miu-planning")
  issuesDf1 <- subset(issuesDf, issuesDf$label.name=="epic")
  issuesDf2 <- subset(issuesDf, issuesDf$label.name=="P0" | issuesDf$label.name=="P1" | issuesDf$label.name=="P2")
  issuesDf2 <- issuesDf2[, c("url", "label.name")]
  colnames(issuesDf2) <- c("url", "priority")
  issuesDf <- merge(x = issuesDf1, issuesDf2, by="url", all = T)
  rm(issuesDf1, issuesDf2)
  
  issuesDf$epic_name <- sapply(X = strsplit(issuesDf$title, ": "), FUN = function(x){x[2]})
  issuesDf$milestone.due_on <- as.numeric(as.Date(issuesDf$milestone.due_on))
  issuesDf$count <- 1
  issuesDf$fill <- ifelse(issuesDf$closed_at=="none",0,1)
  
  df <- issuesDf
  df <- df[order(df$epic_name,  decreasing = T),]
  
  th <- theme(panel.background = element_rect(fill = 'white'),
              legend.position="none",
              axis.text.x = element_text(size=12),
              axis.title.x = element_blank()
  )
  
  plot <- ggplot(data=df)
  plot <- plot + geom_bar(stat="identity", color = "white")
  plot <- plot + facet_grid(priority ~ ., scales = "free", space = "free")
  plot <- plot + aes(x=reorder(milestone.title,milestone.due_on), y=count, fill = factor(fill), label = str_wrap(epic_name, width = 25))
  plot <- plot + theme(legend.position='none')
  plot <- plot + scale_fill_manual(values = c("#757575","#0D47A1"))
  plot <- plot + scale_y_continuous(breaks=c(0,5,10,15))
  plot <- plot + geom_text(size = 3, position = position_stack(vjust = 0.5), color = "white")
  plot <- plot + th
  plot
}
