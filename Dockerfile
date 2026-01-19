FROM rocker/shiny:4.3

# Install system dependencies for sf / tigris
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev

# Install R packages
RUN R -e "install.packages(c('shiny','ggplot2','dplyr','tidyr',\
  'DT','flextable','plotly','sf','tigris','bslib'))"

# Copy Shiny app
COPY App /srv/shiny-server/App

RUN chown -R shiny:shiny /srv/shiny-server

## Fix tirgris issue
RUN mkdir -p /home/shiny/.cache/tigris && \
    chown -R shiny:shiny /home/shiny/.cache


EXPOSE 3838
CMD ["/usr/bin/shiny-server"]