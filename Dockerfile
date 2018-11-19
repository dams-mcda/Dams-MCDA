FROM rocker/shiny

RUN mkdir -p /var/lib/shiny-server/bookmarks/shiny

# install packages
RUN Rscript -e "install.packages('ggplot2')"
RUN Rscript -e "install.packages('dplyr')"
