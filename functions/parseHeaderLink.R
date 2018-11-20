parseHeaderLink <- function(headerLink){
  links <- unlist(strsplit(headerLink, ", "))
  links <- unlist(strsplit(links, "; "))
  links <- gsub(pattern = "<|>", replacement = "", x = links)
  links <- gsub(pattern = "rel=", replacement = "", x = links)
  links <- gsub(pattern = "\"", replacement = "", x = links)
  links <- data.frame(matrix(links, ncol = 2, byrow = T), stringsAsFactors = F)
  names(links) <- c("url", "rel")
  links
}