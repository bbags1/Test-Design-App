# List of required packages
# need `sudo apt-get install -y cmake`
# use custom library in /tmp directory
system("mkdir -p /tmp/shiny-server/R/3.6.3/lib/R/library")
.libPaths("/tmp/shiny-server/R/3.6.3/lib/R/library")

required_packages <- c("aws.s3", "readr", "readxl", "writexl", "jsonlite", "dplyr",
                       "data.table", "ggplot2", "pwr", "tidyverse", "hash", "shiny",
                       "openxlsx", "reticulate", "DT", "shinydashboard", "shinyBS",
                       "corrplot", "httpuv", "Rcpp", "shinyjs", "httr", "bcrypt","skpr")
try({
# Check and install missing packages
for (package in required_packages) {
  if (!requireNamespace(package, quietly = FALSE)) {
    install.packages(package, repos="https://cran.rstudio.com",lib="/tmp/shiny-server/R/3.6.3/lib/R/library")
  }
}
}, silent=TRUE)
install.packages('https://cran.r-project.org/src/contrib/Archive/car/car_3.1-0.tar.gz', repos=NULL,lib="/tmp/shiny-server/R/3.6.3/lib/R/library")
install.packages('https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.5.1.tar.gz', repos=NULL,lib="/tmp/shiny-server/R/3.6.3/lib/R/library")
for (package in required_packages) {
  if (!requireNamespace(package, quietly = FALSE)) {
    install.packages(package, repos="https://cran.rstudio.com",lib="/tmp/shiny-server/R/3.6.3/lib/R/library")
  }
}
# Load all required packages
lapply(required_packages, library, character.only = TRUE)

db_token_value = ""
db_job_id = 1479

# Turn off SSL verification (!! Only do this for trusted URLs !!)
httr::set_config(httr::config(ssl_verifypeer = 0L))

# Get list of completed runs for the specified job:
response_job = httr::GET()
my_runs=httr::content(response_job, "parsed")

# extract the most recent completed run_id
last_run_id=my_runs$runs[[1]]$run_id

# Get results of most recent completed run
response_run = httr::GET()

# extract credentials from results of most recent run
my_creds=httr::content(response_run, "parsed")
creds=jsonlite::fromJSON(my_creds$notebook_output$result)

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = creds$AccessKeyId,
  "AWS_SECRET_ACCESS_KEY" = creds$SecretAccessKey,
  "AWS_SESSION_TOKEN" = creds$Token,
  "AWS_DEFAULT_REGION" = "us-gov-west-1"
)

bucket <<- ""
folder <<- ""

source('modelparams.R')
source('pcg.R')
source('inputFLs.R')
source('powerbysample.R')
source('getfoldernames.R')
source('temgenerator.R')
source('ingestotherfactors.R')
source("remove_string.R")

