#FROM rocker/tidyverse:latest
FROM rocker/tidyverse:3.6.3

RUN R -e "install.packages(c('mlegp','solartime'))"

RUN installGithub.r bgctw/REddyProc \
&& rm -rf /tmp/downloaded_packages/

