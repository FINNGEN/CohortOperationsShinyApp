FROM rocker/r-ver:4.1.2

# install devian dependecies
RUN apt-get update && apt-get install -y git-core libcurl4-openssl-dev libgit2-dev libicu-dev libssl-dev libxml2-dev \
 default-jdk default-jre libpcre2-dev liblzma-dev libbz2-dev \
 make pandoc pandoc-citeproc && rm -rf /var/lib/apt/lists/*

# install R dependecies using renv
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
## set a token to access github
ENV GITHUB_PAT "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"
ENV RENV_VERSION 0.14.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
WORKDIR /project
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'

# open ports
ENV GOLEM_CONFIG_ACTIVE "dev_laptop_javier"
EXPOSE 8888
CMD R -e "options('shiny.port'=8888,shiny.host='0.0.0.0',shiny.maxRequestSize=300*1024^2);CohortOperationsShinyApp::run_app()"