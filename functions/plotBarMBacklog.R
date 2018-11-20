library(ggplot2)
library(plyr)
library(dplyr)
library(stringr)

# ## for troubleshooting
# rm(list=ls())
# load("issuesOpenEpics.RData")
# issuesDf <- issuesOpenEpics

plotBarMBacklog <- function(issuesDf){
  issuesDf <- subset(issuesDf, issuesDf$closed_at=="none")
  issuesDf$epic_name <- sapply(X = strsplit(issuesDf$title, ": "), FUN = function(x){x[2]})
  issuesDf$prefixAlpha <- sapply(X = strsplit(issuesDf$prefix, "-"), FUN = function(x){x[1]})
  issuesDf$count <- 1
  
  df <- issuesDf
  df <- subset(df, df$prefixAlpha=="E")
  df <- df[order(df$epic_name,  decreasing = F),]
  
  th <- theme(panel.background = element_rect(fill = 'white'),
              legend.position="none",
              axis.text.x = element_blank(),
              axis.title.x = element_blank()
  )
  
  plot <- ggplot(data=df)
  plot <- plot + geom_bar(stat="identity", color = "white")
  plot <- plot + aes(x=repo, y=count, fill = factor(count), label = str_wrap(epic_name, width = 30))
  plot <- plot + theme(legend.position='none')
  plot <- plot + scale_fill_manual(values = rep("#757575", nrow(df)))
  plot <- plot + scale_y_reverse()
  plot <- plot + geom_text(size = 3, position = position_stack(vjust = 0.5), color = "white")
  plot <- plot + th
  plot
}
