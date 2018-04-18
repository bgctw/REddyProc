#FROM rocker/tidyverse:latest
FROM rocker/tidyverse:3.4

RUN R -e "install.packages(c('mlegp'))"

RUN installGithub.r bgctw/REddyProc \
&& rm -rf /tmp/downloaded_packages/

