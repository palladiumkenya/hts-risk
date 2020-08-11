FROM rocker/shiny

RUN su - -c "R -e \"install.packages('randomForest', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('dplyr', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('RSQLite', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('shinythemes', repos='https://cran.rstudio.com/')\""

RUN rm -Rf /srv/shiny-server/*

COPY . /srv/shiny-server

WORKDIR /srv/shiny-server

VOLUME ["/srv/shiny-server"]

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]