library(ggplot2)

# ## for troubleshooting
# rm(list=ls())
# load("data/issuesOpen.RData")
# load("data/issuesClosed.RData")
# issues <- rbind(issuesClosed, issuesOpen)
# issuesDf <- issues
# ms <- "Contracts v1.1"

plotBarStory <- function(issuesDf, ms){
  sub <- subset(issuesDf, issuesDf$milestone.title==ms)
  sub <- subset(sub, sub$repo!="RMSone/miu-planning")
  
  # subProd <- subset(sub, sub$closed_at!="none" | sub$label.name %in% c("deployed"))
  subProd <- subset(sub, sub$closed_at!="none")
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
  stories <- cbind(stories[rep(row.names(stories), 4), 1:2], data.frame(env = rep(c("1 - To Do", "2 - On Dev", "3 - On PProd", "4 - On Prod"), each = nrow(stories))))
  
  epics1 <- subset(issuesDf, issuesDf$milestone.title==ms & issuesDf$repo=="RMSone/miu-planning" & issuesDf$label.name=="epic")
  epics1 <- epics1[, c("title", "prefix")]
  epics2 <- subset(issuesDf, issuesDf$milestone.title==ms & issuesDf$repo=="RMSone/miu-planning" & (issuesDf$label.name=="P0" | issuesDf$label.name=="P1" | issuesDf$label.name=="P2"))
  epics2 <- epics2[, c("title", "label.name")]
  
  epics <- merge(x = epics1, y = epics2, by = c("title"))
  epics$epic <- sapply(X = strsplit(epics$title, ": "), FUN = function(x){x[2]})
  epics$number <- sapply(X = strsplit(epics$prefix, "-"), FUN = function(x){x[2]})
  epics$priority <- epics$label.name
  epics <- unique(epics[, c("number", "epic", "priority")])
  rm(epics1, epics2)
  
  df <- merge(x = count, y = countProd, by = "prefix", all = T)
  df <- merge(x = df, y = countPreProd, by = "prefix", all = T)
  df <- merge(x = df, y = countDev, by = "prefix", all = T)
  
  df$to_do <- ifelse(is.na(df$countTotal), 0,df$countTotal) - ifelse(is.na(df$countDev), 0,df$countDev)
  df$on_dev <- ifelse(is.na(df$countDev), 0,df$countDev) - ifelse(is.na(df$countPreProd), 0,df$countPreProd)
  df$on_preprod <- ifelse(is.na(df$countPreProd), 0,df$countPreProd) - ifelse(is.na(df$countProd), 0,df$countProd)
  df$on_prod <- ifelse(is.na(df$countProd), 0,df$countProd)
  
  df <- rbind(data.frame(env = "1 - To Do", prefix = df$prefix, count = df$to_do, stringsAsFactors = F),
              data.frame(env = "2 - On Dev", prefix = df$prefix, count = df$on_dev, stringsAsFactors = F),
              data.frame(env = "3 - On PProd", prefix = df$prefix, count = df$on_preprod, stringsAsFactors = F),
              data.frame(env = "4 - On Prod", prefix = df$prefix, count = df$on_prod, stringsAsFactors = F))
  
  
  df <- merge(x = df, y = stories, by = c("prefix", "env"), all = T)
  df[is.na(df$count), "count"] <- 0
  df$number <- sapply(X = strsplit(df$prefix, "-"), FUN = function(x){x[2]})
  df$number <- sapply(X = strsplit(df$number, "\\."), FUN = function(x){x[1]})
  df <- merge(x = df, y = epics, by = "number", all = T)
    
  df$type <- sapply(X = strsplit(df$prefix, "-"), FUN = function(x){x[1]})

  df$epic <- ifelse(is.na(df$epic), "XX", df$epic)
  df$label <- strtrim(df$title, 35)
  df$env <- factor(df$env)
  df$env <- factor(df$env, levels = rev(levels(df$env)))
  
  plot <- ggplot(data=df) 
  plot <- plot + geom_bar(stat="identity")
  plot <- plot + coord_flip()
  plot <- plot + aes(x=reorder(label, count), y=count, fill=env)
  plot <- plot + scale_fill_manual(values = c("#64B5F6", "#2196F3", "#1976D2", "#0D47A1"))
  plot <- plot + theme(panel.background = element_rect(fill = 'white'))
  plot <- plot + theme(legend.position="bottom")
  plot <- plot + theme(axis.text.x=element_text(size=12))
  plot <- plot + theme(axis.text.y=element_text(size=12, hjust = 0))
  plot <- plot + theme(legend.text=element_text(size=12))
  plot <- plot + theme(axis.title.x=element_blank())
  plot <- plot + theme(axis.title.y=element_blank())
  plot <- plot + theme(legend.title=element_blank())
  plot <- plot + facet_grid(epic ~ ., scales = "free", space = "free")
  plot <- plot + theme(strip.text.y = element_text(angle = 0))
  plot <- plot + theme(strip.background = element_rect(colour = "white"))
  plot
}
