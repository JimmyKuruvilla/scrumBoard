library(ggplot2)

# ## for troubleshooting
# rm(list=ls())
# load("data/issuesOpen.RData")
# load("data/issuesClosed.RData")
# issues <- rbind(issuesClosed, issuesOpen)
# issuesDf <- issues
# ms <- "Miu v2.10"

plotBurn <- function(issuesDf, ms){
  sub <- subset(issuesDf, issuesDf$milestone.title==ms)
  sub <- subset(sub, sub$repo!="RMSone/miu-planning")
  
  sub_u <- unique(sub[, c("repo", "number", "closed_at")])
  sub_c <- subset(sub_u, sub_u$closed_at!="none")
  sub_o <- subset(sub_u, sub_u$closed_at=="none")
  
  start <- as.Date(unique(sub$milestone.description))
  startGraph <- start - 24
  endTarget <- as.Date(unique(sub$milestone.due_on))
  end <- as.Date(max(sub_c$closed_at))
  endGraph <- as.Date(ifelse(is.na(end), endTarget, as.Date(max(end, endTarget))), origin="1970-01-01")
  
  burnTarget <- rbind(sub_o, subset(sub_c, as.Date(sub_c$closed_at)>=start))
  n <- nrow(burnTarget)
  step <- as.integer(endTarget-start)/n
  stepSeq <- seq(from = n, to = 0, by = -step)
  dateSeq <- as.Date(seq(from = start, to = endTarget, length.out = length(stepSeq)))
  burnTarget <- data.frame(date = dateSeq, issues = stepSeq)
  burnTarget$class <- "targetBurn"
  
  burnActual <- rbind(sub_o, subset(sub_c, as.Date(sub_c$closed_at)>=startGraph))
  m <- nrow(burnActual)
  burnActual <- subset(sub_c, as.Date(sub_c$closed_at)>=startGraph)
  
  df <- if(nrow(burnActual)!=0){
    burnActual$closed_at <- as.Date(burnActual$closed_at)
    burnActual <- as.data.frame(table(burnActual$closed_at), stringsAsFactors = F)
    names(burnActual) <- c("date", "issues")
    burnActual$date <- as.Date(burnActual$date)
    burnActual$issues <- as.numeric(m - cumsum(burnActual$issues))
    burnActual <- rbind(burnActual, data.frame(date = startGraph, issues = m))
    burnActual$class <- "actualBurn"
    df <- rbind(burnActual, burnTarget)
    df
  } else {
    df <- rbind(burnTarget)
    df
  }

  plot <- ggplot(data = df)
  plot <- plot + aes(x = date, y = issues, color = class)
  plot <- plot + geom_step(linetype=1, size=2)
  plot <- plot + scale_colour_manual(values=c("#F44336", "#2196F3"))
  plot <- plot + theme(panel.background = element_rect(fill = 'white'))
  plot <- plot + theme(axis.title=element_blank())
  plot <- plot + theme(legend.title = element_blank())
  plot <- plot + theme(axis.text=element_text(size=12))
  plot <- plot + theme(legend.text=element_text(size=12))
  plot <- plot + theme(legend.position="right")
  plot <- plot + geom_vline(aes(xintercept=start),linetype=2,size=1,colour="black")
  plot <- plot + xlim(startGraph, endGraph)
  plot
}

