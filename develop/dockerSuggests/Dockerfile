#FROM rocker/tidyverse:latest
FROM rocker/tidyverse:3.6.3

#RUN R -e "install.packages(c('mlegp'))"

RUN apt-get update && apt-get install -y \
  qpdf \
  joe \
  libnetcdf-dev \
  libudunits2-dev

# https://stackoverflow.com/questions/7541101/logic-of-installation-location-of-r-packages-under-linux
# add the apt-R-library for contributed packages (/usr/lib/R/site-library) to the search path, at least picked up by R started from bash
ENV R_LIBS_SITE /usr/local/lib/R/site-library:/usr/lib/R/site-library

# ENV it not picked up by RStudio, so better append to Renviron but does not work either
# RUN echo R_LIBS_SITE='$R_LIBS_SITE:/usr/lib/R/site-library' >> /etc/R/Renviron

# tried modifying /etc/R/Rprofile.site but test was not set, never called
# RUN echo 'test <- "set in /etc/R/Rprofile.site"'  >> /etc/R/Rprofile.site
# RUN echo '.libPaths(c( "/usr/local/lib/R/site-library","/usr/local/lib/R/library","/usr/lib/R/site-library","/usr/lib/R/library"))' >> /etc/R/Rprofile.site

# Finally this is picked up also by RStudio
#RUN echo 'test <- "set in /home/rstudio/.Rprofile"'  > /home/rstudio/.Rprofile
RUN echo '.libPaths(c( "/usr/local/lib/R/site-library","/usr/local/lib/R/library","/usr/lib/R/site-library","/usr/lib/R/library"))' >> /home/rstudio/.Rprofile

RUN R -e "install.packages(c(\
'mlegp'\
,'ncdf4'\
,'RNetCDF'\
,'minpack.lm'\
,'segmented'\
,'solartime'\
))"

# when building with the stable stack, better install R-packages by install.packages
# instead from apt-get r-cran-x

# ADD data/gapminder-FiveYearData.csv /home/rstudio/
