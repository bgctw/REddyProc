#FROM rocker/tidyverse:latest
FROM rocker/tidyverse:3.4

RUN R -e "install.packages(c('mlegp','REddyProc'))"

