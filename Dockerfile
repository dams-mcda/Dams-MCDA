FROM rocker/shiny:3.5.1

RUN mkdir -p /var/lib/shiny-server/bookmarks/shiny
RUN mkdir -p /srv/matlab-source
RUN mkdir -p /srv/matlab-working-dir

RUN apt-get update
RUN apt-get install -y \
	libssl-dev libcurl4-openssl-dev libssh2-1-dev libpq-dev zlib1g-dev sudo net-tools procps \
	libgdal-dev gdal-bin

# test gdal
RUN gdal-config --version

# install packages
RUN Rscript -e "install.packages('ggplot2')"
RUN Rscript -e "install.packages('dplyr')"
RUN Rscript -e "install.packages('shinyjs')"
RUN Rscript -e "install.packages('Cairo')"
RUN Rscript -e "install.packages('plotly')"
RUN Rscript -e "install.packages('R.matlab')"
RUN Rscript -e "install.packages('rgdal', dependencies=TRUE)"
RUN Rscript -e "install.packages('spatstat')"
RUN Rscript -e "install.packages('rjson')"
RUN Rscript -e "install.packages('leaflet')"
RUN Rscript -e "install.packages('viridis')"
RUN Rscript -e "install.packages('DT')"

# root for folder permissions
USER root

# make sure shiny user has access to server
RUN chown -R shiny:shiny /srv/shiny-server && chmod -R 775 /srv/shiny-server
RUN chown -R shiny:shiny /var/log/shiny-server && chmod -R 775 /var/log/shiny-server
RUN chown -R shiny:shiny /srv/matlab-working-dir && chmod -R 775 /srv/matlab-working-dir

RUN addgroup docker
RUN adduser shiny docker
# install folder permissions
RUN chown root:shiny -R /usr/local

RUN usermod -u 1000 shiny

USER shiny
WORKDIR /
# set entrypoint in docker-compose for switching entrypoint without rebuilding
