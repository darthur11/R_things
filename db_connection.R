#DBs:
library(RODBC)
library(RMariaDB)
library(RPostgres)



##### Connections to different databases -----
credentials <- readRDS('C:/Users/Artur.Dossatayev/Documents/credentials/cred.RDS')
mssql <- odbcDriverConnect(paste0('driver={SQL Server};server=',
                               credentials$mssql$local, 
                               ';database=',
                               credentials$mssql$db,
                               ';uid=', 
                               credentials$mssql$login, 
                               ';pwd=', 
                               credentials$mssql$password,
                               ';'))

redshift <- dbConnect(RPostgres::Postgres(), 
                    host=credentials$redshift$server,
                    port=credentials$redshift$port,
                    user=credentials$redshift$login,
                    dbname=credentials$redshift$db,
                    password=credentials$redshift$password)

mysql <- dbConnect(MariaDB() ,user=mysql$mysql$login,
                    port = credentials$mysql$port,   
                    host=credentials$mysql$server,
                    dbname=credentials$mysql$db,
                    password=credentials$mysql$password)


