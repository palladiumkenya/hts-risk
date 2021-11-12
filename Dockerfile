FROM rocker/shiny

RUN apt-get update -y && apt-get install nginx libmysqlclient-dev -y

RUN su - -c "R -e \"install.packages('caret', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('randomForest', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('dplyr', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('RSQLite', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('shinythemes', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('shinyjs', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('lubridate', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('xgboost', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('DBI', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('RMariaDB', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('config', repos='https://cran.rstudio.com/')\""



RUN rm -Rf /srv/shiny-server/*

COPY . /srv/shiny-server

RUN chown -R shiny:shiny /srv/shiny-server

WORKDIR /srv/shiny-server

VOLUME ["/srv/shiny-server"]

EXPOSE 3838

CMD ["/bin/bash", "-c", "/usr/sbin/nginx && /usr/bin/shiny-server"]

CMD ["/usr/bin/shiny-server"]
