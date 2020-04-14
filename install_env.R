dynamic_require <- function(package){
  if(eval(parse(text=paste("require(",package,")")))) { return(TRUE) }
  install.packages(package)
  return(eval(parse(text=paste("require(",package,")"))))
}

packages<-c('dplyr','tidyr','tidyverse','devtools', 'ggplot2', 'stringi', 'stringr', 'magrittr',
            'RMariaDB','lubridate','slackr','TTR','googledrive','openxlsx')

for (pkg in packages) {
  dynamic_require(pkg)
}


devtools::install_github('tidyverse/googlesheets4')


install.packages("compareDF")
