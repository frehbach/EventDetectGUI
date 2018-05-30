
getAmountCsvCheckBoxes <- function(){
    return(ncol(getEnvData("csvData")))
}

createCsvCheckBoxes <- function(){
    csvdata <- getEnvData("csvData")
    checkboxGroupInput(inputId = "csvColumnCheckBox",label = "",
                       choices = colnames(csvdata)[-1],
                       selected = colnames(csvdata)[-1],
                       inline = TRUE)
}

setAllCsvCheckBoxes <- function(session){
    csvdata <- getEnvData("csvData")
    updateCheckboxGroupInput(session,"csvColumnCheckBox",selected = colnames(csvdata)[-1])
}

resetAllCsvCheckBoxes <- function(session){
    updateCheckboxGroupInput(session,"csvColumnCheckBox",selected = character(0))
}
