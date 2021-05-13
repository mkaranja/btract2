suppressPackageStartupMessages(library(shiny, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(shinydashboard, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(shinyWidgets, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(shinycssloaders, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(shinyBS, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(shinyalert, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(jsonlite, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(shinyjs, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(DT, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(data.table, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(summarytools, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(qrencoder, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(highcharter, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(collapsibleTree, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(magrittr, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(tidyverse, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(plyr, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(WriteXLS, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(dataframes2xls, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(stringr, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(vroom, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(plotly, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(Hmisc, warn.conflicts=FALSE))
suppressPackageStartupMessages(library(tidyverse, warn.conflicts=FALSE))
enableBookmarking(store = "url")

source("dbConnect.R", local=T)

# Tabs
tab_files <- list.files(path = "tabs/ui", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"

