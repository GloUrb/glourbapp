FROM rocker/r2u:jammy

LABEL org.opencontainers.image.authors="Lise Vaudor <lise.vaudor@ens-lyon.fr>, Samuel Dunesme <samuel.dunesme@ens-lyon.fr>"
LABEL org.opencontainers.image.source="https://github.com/glourb/glourbapp"
LABEL org.opencontainers.image.documentation="https://evs-gis.github.io/glourbdoc/"
LABEL org.opencontainers.image.version="0.0.0.9000"
LABEL org.opencontainers.image.description="An app for the GloUrb project"

RUN locale-gen fr_FR.UTF-8

RUN Rscript -e 'install.packages("tidyr")'
RUN Rscript -e 'install.packages("shiny")'
RUN Rscript -e 'install.packages("config")'
RUN Rscript -e 'install.packages("testthat")'
RUN Rscript -e 'install.packages("tibble")'
RUN Rscript -e 'install.packages("purrr")'
RUN Rscript -e 'install.packages("dplyr")'
RUN Rscript -e 'install.packages("ggplot2")'
RUN Rscript -e 'install.packages("forcats")'
RUN Rscript -e 'install.packages("magrittr")'
RUN Rscript -e 'install.packages("golem")'
RUN Rscript -e 'install.packages("sf")'
RUN Rscript -e 'install.packages("plotly")'
RUN Rscript -e 'install.packages("spelling")'
RUN Rscript -e 'install.packages("mclust")'
RUN Rscript -e 'install.packages("leaflet")'
RUN Rscript -e 'install.packages("cluster")'
RUN Rscript -e 'install.packages("cowplot")'
RUN Rscript -e 'install.packages("FactoMineR")'
RUN Rscript -e 'install.packages("glue")'
RUN Rscript -e 'install.packages("grDevices")'
RUN Rscript -e 'install.packages("RColorBrewer")'
RUN Rscript -e 'install.packages("remotes")'
RUN Rscript -e 'install.packages("cicerone")'
RUN Rscript -e 'install.packages("RPostgres")'
RUN Rscript -e 'install.packages("DBI")'
RUN Rscript -e 'install.packages("DT")'
RUN Rscript -e 'install.packages("bslib")'

RUN Rscript -e 'remotes::install_github("glourb/glourbi")'
RUN Rscript -e 'remotes::install_github("glourb/gsw")'

RUN mkdir /app
ADD . /app
WORKDIR /app

RUN R -e 'remotes::install_local()'

EXPOSE 3838

RUN groupadd -g 1010 app && useradd -c 'app' -u 1010 -g 1010 -m -d /home/app -s /sbin/nologin app
USER app

CMD  ["R", "-f", "app.R"]
