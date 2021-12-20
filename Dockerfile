FROM rocker/r-ver:4.1.2

LABEL name="CohortOperationsShinyApp"
LABEL maintainer="javier.graciatabuenca@tuni.fi"

# PARAMETERS
## Used during building
ARG GITHUB_PAT
ENV GITHUB_PAT $GITHUB_PAT
## Used during runtime
ENV port="8888"
ENV host="0.0.0.0"
ENV GOLEM_CONFIG_ACTIVE="production"

# BUILDING
## Install devian dependecies
RUN apt-get update && apt-get install -y git-core libcurl4-openssl-dev libgit2-dev libicu-dev libssl-dev libxml2-dev \
 default-jdk default-jre libpcre2-dev liblzma-dev libbz2-dev \
 make pandoc pandoc-citeproc && rm -rf /var/lib/apt/lists/*

## Copy project
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone

## Install shiny app
### seting dependencies using renv
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
ENV RENV_PATHS_CACHE /build_zone/renv/cache
RUN R -e 'renv::restore(); renv::isolate()'
### install this project (TMP for some reason stringr has to be re installed)
RUN R -e 'renv::rebuild("stringi", recursive=FALSE);renv::install(".")'
### move library to container
RUN R -e 'renv::deactivate(); file.copy(list.files(renv::paths$library(), full.names = TRUE), .libPaths()[1], recursive=TRUE)'

## Clean up built
ENV GITHUB_PAT 0
RUN rm -rf /build_zone

# RUNTIME
EXPOSE $port
CMD R -e "options(shiny.port=$port,shiny.host='$host');CohortOperationsShinyApp::run_app()"
