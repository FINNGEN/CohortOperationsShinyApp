FROM rocker/r-ver:4.1.2

LABEL name="CohortOperationsShinyApp"
LABEL maintainer="javier.graciatabuenca@tuni.fi"

# ENV variables,
# used during building
ARG GITHUB_PAT
ENV GITHUB_PAT $GITHUB_PAT
# used during runtime
ENV port="8888"
ENV host="0.0.0.0"
ENV GOLEM_CONFIG_ACTIVE="production"

# install devian dependecies
RUN apt-get update && apt-get install -y git-core libcurl4-openssl-dev libgit2-dev libicu-dev libssl-dev libxml2-dev \
 default-jdk default-jre libpcre2-dev liblzma-dev libbz2-dev \
 make pandoc pandoc-citeproc && rm -rf /var/lib/apt/lists/*

# copy project
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone

# install R dependecies using renv
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
ENV RENV_VERSION 0.14.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
RUN R -e 'renv::restore()'

# install project
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone

# Run time
EXPOSE $port
CMD R -e "options('shiny.port'=$port,shiny.host='$host');CohortOperationsShinyApp::run_app()"
