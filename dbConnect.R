
suppressPackageStartupMessages(library(odbc, warn.conflicts=FALSE))

pool <- dbConnect(odbc(),
                  Driver = "ODBC Driver 17 for SQL Server",
                  Server = "52.212.46.173\\SQLEXPRESS,41433",
                  Database = "BTRACT",
                  UID = "mkaranja",
                  PWD = "a1s2d3"
)

