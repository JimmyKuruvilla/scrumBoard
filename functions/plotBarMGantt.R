library(ggplot2)
library(plyr)

# ## for troubleshooting
# rm(list=ls())
# load("issuesOpen.RData")
# load("issuesClosed.RData")
# issues <- rbind(issuesClosed, issuesOpen)
# issuesDf <- issues
# milestonesDf <- milestones
# ms <- "Contracts v0.5"

plotBarMGantt <- function(issuesDf, milestonesDf, ms){
  m <- unique(milestonesDf[, c("title", "description")])
  m$description <- as.Date(m$description)
  d <- subset(m, m$title==ms)$description
  m <- subset(m, m$description>=d)
  
  issuesDf <- subset(issuesDf, issuesDf$milestone.title %in% m$title)
  issuesDf <- subset(issuesDf, issuesDf$repo!="RMSone/miu-planning")
  
  t <- as.Date(Sys.Date())
  df <- ddply(issuesDf, ~milestone.title, summarise, 
              start = as.Date(max(milestone.description)), 
              end = as.Date(max(milestone.due_on)), 
              endA = as.Date(ifelse(max(closed_at)=="none" & as.Date(Sys.Date())<=as.Date(max(milestone.due_on)), as.Date(max(milestone.due_on)), 
                                    ifelse(max(closed_at)=="none" & as.Date(Sys.Date())>as.Date(max(milestone.due_on)), as.Date(Sys.Date()), 
                                           as.Date(max(closed_at)))), origin = "1970-01-01")
              )
  
  s <- as.Date(min(df$start))
  
  df$phase0 <- as.numeric(df$start - s)
  df$phase1 <- as.numeric(ifelse(t<=df$start, 0,
                                 ifelse(df$start<t & t<=df$end, t - df$start,
                                        ifelse(df$end<t & t<=df$endA, df$end-df$start, df$end-df$start))))
  df$phase2 <- as.numeric(ifelse(t<=df$start, df$end-df$start,
                                 ifelse(df$start<t & t<=df$end, df$end - t,
                                        ifelse(df$end<t & t<=df$endA, 0, 0))))
  df$phase3 <- as.numeric(df$endA - df$end)
  
  df_phase0 <- data.frame(ms = df$milestone.title, date = as.numeric(df$endA), count = df$phase0, phase = 0, stringsAsFactors = F)
  df_phase1 <- data.frame(ms = df$milestone.title, date = as.numeric(df$endA), count = df$phase1, phase = 1, stringsAsFactors = F)
  df_phase2 <- data.frame(ms = df$milestone.title, date = as.numeric(df$endA), count = df$phase2, phase = 2, stringsAsFactors = F)
  df_phase3 <- data.frame(ms = df$milestone.title, date = as.numeric(df$endA), count = df$phase3, phase = 3, stringsAsFactors = F)
  
  df_plot <- rbind(df_phase0, df_phase1, df_phase2, df_phase3)
  df_plot$phase <- as.factor(df_plot$phase)
  df_plot$label <- ifelse(df_plot$phase!=0, "", sapply(X = strsplit(df_plot$ms, " "),FUN = function(x){x[2]}))
  df_plot <- arrange(df_plot, ms, phase)
  df_plot <- ddply(df_plot,"ms",transform,pos =cumsum(count))
  df_plot$labelx <- ifelse(df_plot$phase!=3,0,df_plot$pos)
  
  th <- theme(panel.background = element_rect(fill = 'white'),
             legend.position="none",
             axis.text.x = element_text(size=12, angle = 30, hjust = 1),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             panel.grid.major.x = element_line(colour = "black" ,size = 0.5, linetype = 2)
             )
  
  plot <- ggplot(data=df_plot)
  plot <- plot + geom_bar(stat="identity")
  plot <- plot + coord_flip()
  plot <- plot + aes(x=reorder(ms,-date), y=count, fill=factor(phase, levels = c(3,2,1,0)))
  plot <- plot + scale_fill_manual(values = alpha(c("#64B5F6", "#757575", "#0D47A1", "#ffffff"), c(1,1,1,0)))
  plot <- plot + geom_text(aes(y = pos, label=label), color="white", size=5, hjust=-0.2, check_overlap = T)
  plot <- plot + scale_x_discrete(breaks=NULL)
  plot <- plot + scale_y_continuous(breaks = seq(from = max(df_plot$pos), to = 0, by = -84), labels = as.Date.numeric(seq(from = max(df_plot$pos), to = 0, by = -84), origin = s))
  plot <- plot + th
  plot
}
