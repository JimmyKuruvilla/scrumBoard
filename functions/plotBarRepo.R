library(ggplot2)

# ## for troubleshooting
# rm(list=ls())
# load("issuesOpen.RData")
# load("issuesClosed.RData")
# issues <- rbind(issuesClosed, issuesOpen)
# issuesDf <- issues
# ms <- "Contracts v1.0"

plotBarRepo <- function(issuesDf, ms){
  sub <- subset(issuesDf, issuesDf$milestone.title==ms)
  sub <- subset(sub, !(sub$repo %in% c("RMSone/miu-planning")))
  
  # subProd <- subset(sub, sub$closed_at!="none" | sub$label.name %in% c("deployed"))
  subProd <- subset(sub, sub$closed_at!="none")
  subPreProd <- subset(sub, sub$closed_at!="none" | sub$label.name %in% c("deployed", "on preprod", "deploy to prod"))
  subDev <- subset(sub, sub$closed_at!="none" | sub$label.name %in% c("deployed", "on preprod", "deploy to prod", "on upgrade", "on c_upgrade", "on dev", "testing", "deploy to preprod"))
  
  sub <- unique(sub[, c("repo", "number")])
  subProd <- unique(subProd[, c("repo", "number")])
  subPreProd <- unique(subPreProd[, c("repo", "number")])
  subDev <- unique(subDev[, c("repo", "number")])
  
  count <- as.data.frame(table(sub[, "repo"]), stringsAsFactors = F)
  countProd <- as.data.frame(table(subProd[, "repo"]), stringsAsFactors = F)
  countPreProd <- as.data.frame(table(subPreProd[, "repo"]), stringsAsFactors = F)
  countDev <- as.data.frame(table(subDev[, "repo"]), stringsAsFactors = F)
  
  colnames(count) <- c("repo", "countTotal")
  
  if(nrow(countProd)!=0){colnames(countProd) <- c("repo", "countProd")}else{countProd <- data.frame(repo = count$repo, countProd = rep(0, nrow(count)), stringsAsFactors = F)}
  if(nrow(countPreProd)!=0){colnames(countPreProd) <- c("repo", "countPreProd")}else{countPreProd <- data.frame(repo = count$repo, countPreProd = rep(0, nrow(count)), stringsAsFactors = F)}
  if(nrow(countDev)!=0){colnames(countDev) <- c("repo", "countDev")}else{countDev <- data.frame(repo = count$repo, countDev = rep(0, nrow(count)), stringsAsFactors = F)}
  
  df <- merge(x = count, y = countProd, by = "repo", all = T)
  df <- merge(x = df, y = countPreProd, by = "repo", all = T)
  df <- merge(x = df, y = countDev, by = "repo", all = T)
  
  df$to_do <- ifelse(is.na(df$countTotal), 0,df$countTotal) - ifelse(is.na(df$countDev), 0,df$countDev)
  df$on_dev <- ifelse(is.na(df$countDev), 0,df$countDev) - ifelse(is.na(df$countPreProd), 0,df$countPreProd)
  df$on_preprod <- ifelse(is.na(df$countPreProd), 0,df$countPreProd) - ifelse(is.na(df$countProd), 0,df$countProd)
  df$on_prod <- ifelse(is.na(df$countProd), 0,df$countProd)
  
  df <- rbind(data.frame(env = "1 - To Do", repo = df$repo, count = df$to_do, stringsAsFactors = F),
              data.frame(env = "2 - On Dev", repo = df$repo, count = df$on_dev, stringsAsFactors = F),
              data.frame(env = "3 - On PProd", repo = df$repo, count = df$on_preprod, stringsAsFactors = F),
              data.frame(env = "4 - On Prod", repo = df$repo, count = df$on_prod, stringsAsFactors = F))
  
  df$env <- factor(df$env)
  df$env <- factor(df$env, levels = rev(levels(df$env)))
  
  plot <- ggplot(data=df)
  plot <- plot + geom_bar(stat="identity")
  plot <- plot + coord_flip()
  plot <- plot + aes(x=reorder(repo, count), y=count, fill=env)
  plot <- plot + scale_fill_manual(values = c("#64B5F6", "#2196F3", "#1976D2", "#0D47A1"))
  plot <- plot + theme(panel.background = element_rect(fill = 'white'))
  plot <- plot + theme(legend.position="bottom")
  plot <- plot + theme(axis.text=element_text(size=12))
  plot <- plot + theme(legend.text=element_text(size=12))
  plot <- plot + theme(axis.title.x=element_blank())
  plot <- plot + theme(axis.title.y=element_blank())
  plot <- plot + theme(legend.title=element_blank())
  plot
}