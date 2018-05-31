#FROM rocker/tidyverse:latest
FROM rocker/tidyverse:3.4

# need to specify repos, else the version from mran is used with versioned stack
RUN R -e "install.packages(c('mlegp','REddyProc'), repos = 'https://cloud.r-project.org')"


