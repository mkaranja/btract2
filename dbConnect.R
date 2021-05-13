
suppressPackageStartupMessages(library(odbc, warn.conflicts=FALSE))

pool <- dbConnect(odbc(),
                  Driver = "ODBC Driver 17 for SQL Server",
                  Server = "****",
                  Database = "***",
                  UID = "***",
                  PWD = "***"
)

