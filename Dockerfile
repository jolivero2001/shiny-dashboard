FROM r-base:latest

MAINTAINER "jolivero2001@yahoo.com"

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev \
    libssl-dev

# Download and install shiny server..
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

RUN sudo apt-get install libmysqlclient-dev
RUN R -e "install.packages(c('shiny', 'ggplot2', 'ggvis', 'shinydashboard', 'dplyr','shinyjs','lazyeval','shinyAce','knitr','RMySQL','dbConnect'), repos='http://cran.rstudio.com/')"

COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf

CMD ["cd /usr/bin/shiny-server"]
CMD ["mkdir uned"]
COPY /myapp /srv/shiny-server/uned/

EXPOSE 80

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
