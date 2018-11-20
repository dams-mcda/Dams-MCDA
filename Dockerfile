FROM rocker/shiny:3.5.1

RUN mkdir -p /var/lib/shiny-server/bookmarks/shiny

# install packages
RUN Rscript -e "install.packages('ggplot2')"
RUN Rscript -e "install.packages('dplyr')"

RUN chown -R shiny:shiny /srv/shiny-server && chmod -R 775 /srv/shiny-server
RUN addgroup docker
RUN adduser shiny docker
ENTRYPOINT shiny-server.sh
