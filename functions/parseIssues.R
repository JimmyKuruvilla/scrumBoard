parseIssues <- function(issuesList){
  empty_row <- c(url = "none", repo = "none", number = "none", title = "none", prefix = "none", created_at = "none", closed_at = "none", pull_request = "none", milestone.title = "none", milestone.due_on = "none", milestone.description = "none", label.name = "none")
  n <- length(issuesList)
  df <- if(n==0){
    return(empty_row)
    } else {
      parse2 <- function(i){
        m <- length(issuesList[[i]])
        if(m==0){
          return(empty_row)
          } else {
            parse3 <- function(j){
              url <- issuesList[[i]][[j]]$url
              repo <- paste0(strsplit(url, "/")[[1]][5], "/", strsplit(url, "/")[[1]][6])
              number <- issuesList[[i]][[j]]$number
              title <- issuesList[[i]][[j]]$title
              prefix <- if(length(strsplit(title, ":")[[1]])>1){strsplit(title, ":")[[1]][1]}else{"none"}
              created_at <- issuesList[[i]][[j]]$created_at
              closed_at <- if(is.null(issuesList[[i]][[j]]$closed_at)){"none"}else{issuesList[[i]][[j]]$closed_at}
              pull_request <- if(is.null(issuesList[[i]][[j]]$pull_request)){"0"}else{"1"}
              l <- length(issuesList[[i]][[j]]$milestone)
              if(l==0){
                return(c(url = url, repo = repo, number = number, title = title, prefix = prefix, created_at = created_at, closed_at = closed_at, pull_request = pull_request, milestone.title = "none", milestone.due_on = "none", milestone.description = "none", label.name = "none"))
                } else {
                  parse4 <- function(){
                    milestone.title <- issuesList[[i]][[j]]$milestone$title
                    milestone.due_on <- if(is.null(issuesList[[i]][[j]]$milestone$due_on)){"none"}else{issuesList[[i]][[j]]$milestone$due_on}
                    milestone.description <- if(is.null(issuesList[[i]][[j]]$milestone$description)){"none"}else{issuesList[[i]][[j]]$milestone$description}
                    p <- length(issuesList[[i]][[j]]$labels)
                    if(p==0){
                      return(c(url = url, repo = repo, number = number, title = title, prefix = prefix, created_at = created_at, closed_at = closed_at, pull_request = pull_request, milestone.title = milestone.title, milestone.due_on = milestone.due_on, milestone.description = milestone.description, label.name = "none"))
                      } else {
                        parse5 <- function(k){
                          label.name <- issuesList[[i]][[j]]$labels[[k]]$name
                          return(c(url = url, repo = repo, number = number, title = title, prefix = prefix, created_at = created_at, closed_at = closed_at, pull_request = pull_request, milestone.title = milestone.title, milestone.due_on = milestone.due_on, milestone.description = milestone.description, label.name = label.name))
                        }
                        lapply(X = c(1:p), FUN = parse5)
                      }
                  }
                  parse4()
                }
            }
            lapply(X = c(1:m), FUN = parse3)
          }
      }
      lapply(X = c(1:n), FUN = parse2)
    }
  df <- data.frame(matrix(unlist(df), ncol = 12, byrow = T), stringsAsFactors = F)
  colnames(df) <- c("url", "repo", "number", "title", "prefix", "created_at", "closed_at", "pull_request", "milestone.title", "milestone.due_on", "milestone.description", "label.name")
return(df)
}