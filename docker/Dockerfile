# build: docker build -f ./Dockerfile -t sherver .
# run: docker run -it -p 3838:3838 -v $(pwd):/srv/shiny-server/ sherver [CMD OVERRIDE]

FROM ubuntu:18.04

WORKDIR /srv/shiny-server

RUN apt-get update && apt-get install -y sudo wget gnupg2 ca-certificates libcurl4-openssl-dev libssl-dev gdebi-core

# Add key for R repository maintainer and add r-project to sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 && \
  echo "deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/" >> /etc/apt/sources.list

# Install R runtime
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
  r-base \
  r-base-dev

# Install Shiny web framework, and server dependencies
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'flexdashboard', 'here', 'knitr', 'plyr', 'dplyr', 'ggplot2', 'stringr'))"

# Download and install Shiny-Server
RUN wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.9.923-amd64.deb && \
  gdebi -n shiny-server-1.5.9.923-amd64.deb

EXPOSE 3838

RUN chown shiny /var/lib/shiny-server/

USER shiny

CMD ["shiny-server"]
