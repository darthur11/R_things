#install.packages("tesseract")
#tesseract_download("tesseract")
library(tesseract)
recognize <- function(file_path) {
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully

      ocr(image = file_path, engine = tesseract("rus"))
      # The return value of `readLines()` is the actual value 
      # that will be returned in case there is no condition 
      # (e.g. warning or error). 
      # You don't need to state the return value via `return()` as code 
      # in the "try" part is not wrapped insided a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      print(paste("Something went wrong:", file_path))
      Sys.sleep(.5)
      return('NA')
    },
    warning=function(cond) {
      print(paste("Warning is here:", file_path))
      Sys.sleep(.5)
      return('NA')
    }
  )    
  return(out)
}
