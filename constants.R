# when running interactively in R, replace the env var lookup with a string of form 'token [TOKEN]'

library('stringr')
env = new.env()
env$token <- str_interp('token ${Sys.getenv("GH_TOKEN")}')