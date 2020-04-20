
> Comparing Disproportionately Black Versus All Other Counties 
> to Assess COVID-19’s Impact on Black Communities

**Authors:** Gregorio A. Millett, MPH, 
Austin T. Jones, MA,
David  Benkeser, PhD,
Stefan Baral, MD, MPH, MBA,
Laina Mercer, PhD,
Chris Beyrer, MD, MPH,
Brian Honermann, JD,
Elise Lankiewicz, MPH,
Leandro Mena, MD, MPH,
Jeffrey S. Crowley, MPH,
Jennifer Sherwood, MSPH,
Patrick Sullivan, DVM, PhD

-----

## Data and code 

This repository contains the data and code needed to reproduce the tables and 
figures included in the paper. The `/Data` directory contains two data files. 
`Data/coviddata.csv` contains county-level information on demographics, underlying
health conditions, social distancing, and COVID-19 cases/deaths as of April 16,
2020. `Data/covid_confirmed_usafacts_416.csv` contains longitudinal data on 
cases occurring over time in each county, which was used in the analysis to 
account for time since first detection in each county. The `Data` directory
also includes state-level shape files [downloaded from the Census Bureau](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2018.html). 

The `Code/` directory contains all `R` code needed to run the analysis. The main
file is `report.Rmd`, an `R` Markdown file that produces all tables and figures 
reported in the manuscript. This `R` Markdown file `source`s all other `R` scripts
included in the `Code` directory. 

-----

## Docker container

The analysis relies on the `R` package `inla`, which has several complex dependent 
packages. For convenience, we built a Docker image described in the `Dockerfile`. 
The image is [available on DockerHub](https://hub.docker.com/repository/docker/dbenkeser/race-and-covid). With Docker installed, you can download the image via the 
command line as follows. 

``` bash
docker pull dbenkeser/covid-and-race:latest
```

Once downloaded, the image can be run using the `docker run` command. In the container,
results are saved to `/County_COVID/Results`. You may wish to [mount a local directory](https://docs.docker.com/storage/volumes/) to the `/County_COVID` directory 
in the container, so that you can open results outside the container. For example,
to run the container with mounted local directory `/path/to/local/directory`, you 
can execute the following at the command line.

``` bash 
docker run -it \
  -v /path/to/local/directory:/County_COVID \
  dbenkeser/race-and-covid:latest
```

The entry point for the container is a bash script. To compile the `R` Markdown document, 
you can execute the following at the command line inside the container. 

``` bash
Rscript -e 'rmarkdown::render("/County_COVID/Code/report.Rmd")'
```

After the report compiles you should see a document `/path/to/local/directory/Results/report.docx` that contains tables from the paper. The folder `/path/to/local/directory/Results/` should also include the figures from the paper as well. Note that in order
to run the analysis, it may be necessary to [increase the memory available to Docker](https://docs.docker.com/docker-for-mac/#resources). 

-----

## Problems?

Please [file an issue](https://github.com/benkeser/race-and-covid/issues) if you have any difficulty accessing these materials. 
