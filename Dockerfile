
FROM rocker/shiny:4.1.0

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

RUN R -e 'install.packages(c(\
              "shiny", "dplyr", "ks", "readr", "readxl", "scales", "ggplot2", "plotly"\
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2021-07-23"\
          )'


COPY ./* /srv/shiny-server/

CMD ["/usr/bin/shiny-server"]

