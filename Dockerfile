FROM rocker/shiny

RUN apt-get update -y && apt-get install libmysqlclient-dev -y

RUN su - -c "R -e \"install.packages('randomForest', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('dplyr', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('DBI', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('RMariaDB', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('shinythemes', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('config', repos='https://cran.rstudio.com/')\""

RUN rm -Rf /srv/shiny-server/*

COPY . /srv/shiny-server

WORKDIR /srv/shiny-server

VOLUME ["/srv/shiny-server"]

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
