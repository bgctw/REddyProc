FROM rocker/tidyverse:latest

RUN R -e "install.packages('mlegp')"
RUN R -e "install.packages('logitnorm')"

# ADD data/gapminder-FiveYearData.csv /home/rstudio/