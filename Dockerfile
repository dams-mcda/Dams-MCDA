FROM rocker/shiny:3.5.1

RUN mkdir -p /var/lib/shiny-server/bookmarks/shiny
RUN apt-get update
RUN apt-get install -y libssl-dev libcurl4-openssl-dev libssh2-1-dev libpq-dev zlib1g-dev

# install packages
RUN Rscript -e "install.packages('ggplot2')"
RUN Rscript -e "install.packages('dplyr')"
RUN Rscript -e "install.packages('shinyjs')"
RUN Rscript -e "install.packages('Cairo')"
RUN Rscript -e "install.packages('plotly')"

RUN chown -R shiny:shiny /srv/shiny-server && chmod -R 775 /srv/shiny-server
RUN addgroup docker
RUN adduser shiny docker
ENTRYPOINT shiny-server.sh
