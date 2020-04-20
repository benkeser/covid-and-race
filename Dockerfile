# Start by pulling ubuntu image
FROM rocker/tidyverse:latest

# non-interactive mode
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get install -y libudunits2-dev
RUN apt-get install -y libgdal-dev

# install R libraries needed for analysis
RUN Rscript -e 'install.packages("sf", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("spdep", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("rgdal", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("maptools", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("maps", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("readr", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("ggthemes", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("spatialreg", repos="https://cran.rstudio.com")'

RUN Rscript -e 'install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)'

RUN apt-get install -y mesa-common-dev
RUN apt-get install -y libglu1-mesa-dev freeglut3-dev

RUN Rscript -e 'install.packages("rgl", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("labelled", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("ggsci", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("plotly", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("htmlWidgets", repos="https://cran.rstudio.com")'

RUN mkdir /County_COVID
RUN mkdir /County_COVID/Results
RUN mkdir /County_COVID/Code
RUN mkdir /County_COVID/Data

COPY Code/ /County_COVID/Code/
COPY Data/ /County_COVID/Code/

# entry point to container opens bash
CMD /bin/bash
