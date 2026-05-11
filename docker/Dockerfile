FROM rocker/r-ver:latest

RUN apt-get update && apt-get install -y \
    nginx \
    pandoc \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e "install.packages(c('rmarkdown', 'knitr', 'data.table', 'googlesheets4'))"

COPY nginx.conf /etc/nginx/sites-enabled/default
COPY start.sh /start.sh

RUN chmod +x /start.sh

EXPOSE 80

CMD ["/start.sh"]