FROM rocker/shiny

RUN apt-get update -y && apt-get install nginx libmysqlclient-dev -y

RUN su - -c "R -e \"install.packages('randomForest', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('dplyr', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('DBI', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('RMariaDB', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('RSQLite', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('shinythemes', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('config', repos='https://cran.rstudio.com/')\""
RUN su - -c "R -e \"install.packages('lubridate', repos='https://cran.rstudio.com/')\""

RUN rm -Rf /srv/shiny-server/*
RUN rm /etc/nginx/sites-enabled/default
RUN echo "server { listen 80 default_server; listen [::]:80 ipv6only=on; root /srv/shiny-server; index index.html index.htm; server_name _; location / { proxy_pass http://127.0.0.1:3838; } }" > /etc/nginx/conf.d/default.conf

# uncomment for HTTPS
# COPY secrets/server.key /etc/ssl/private/server.key
# COPY secrets/server.crt /etc/ssl/certs/server.cert
# RUN echo "server { listen 80; listen [::]:80 ipv6only=on; return 302 https://$server_name$request_uri; }  server { listen 443 ssl default_server; listen [::]:443 ssl default_server; root /srv/shiny-server; index index.html index.htm; server_name _; ssl_certificate /etc/ssl/certs/server.cert; ssl_certificate_key /etc/ssl/private/server.key; ssl_protocols TLSv1 TLSv1.1 TLSv1.2; ssl_ciphers HIGH:!aNULL:!MD5; location / { proxy_pass http://127.0.0.1:3838; } }" > /etc/nginx/conf.d/default.conf

COPY . /srv/shiny-server

RUN chown -R shiny:shiny /srv/shiny-server

WORKDIR /srv/shiny-server

VOLUME ["/srv/shiny-server"]

EXPOSE 80

EXPOSE 443

CMD ["/bin/bash", "-c", "/usr/sbin/nginx && /usr/bin/shiny-server"]
