#' Generate Server Part of owosGUI
#'
#' Generates the server part of the owosGUI.
#' This method is used internally in the starting process of the GUI.
#' Manual use of this function is not advised.
#'
#' @param input shiny UI-input
#' @param output shiny UI-output
#' @param session shiny UI-session
#' @export
getServer <- function(input, output, session) {

    #Reads the currently configured csv file and shows its head.
    #Initial Data loading
    output$outDataHead <- DT::renderDataTable({
        req(input$inputFile)
        setEnvData("csvData",read.csv(input$inputFile$datapath,
                             header = input$csvUseHeader,
                             sep = input$csvSep,
                             quote = input$csvQuote))
        return(getEnvData("csvData"))
    },options = list(scrollX = TRUE,
                     pageLength = 3,
                     lengthMenu = c(3, 5, 10, 15, 20)))

    #Update the data table whenever a checkbox is changed.
    observeEvent(input$csvColumnCheckBox,{
        req(input$inputFile)
        cols <- which(names(getEnvData("csvData")[-1]) %in% input$csvColumnCheckBox)
        cols <- cols + 1
        if(length(cols) >= 1){
            cols <- c(1,cols)
        }
        output$outDataHead <- DT::renderDataTable(getEnvData("csvData")[,cols],
                                                  options = list(scrollX = TRUE,
                                                                pageLength = length(input$outDataHead_rows_current),
                                                                lengthMenu = c(3, 5, 10, 15, 20)))
    }, ignoreNULL = FALSE)

    output$outDataSelection <- renderUI({
        req(input$inputFile)
        elementList <- list(
            wellPanel(
                checkboxInput("csvSelectColumns", "Dis-/Enable data columns", FALSE),
                # if the ceckbox "csvSelectColumns" is true, a new panel will pop up
                conditionalPanel(
                    condition = "input.csvSelectColumns == true",
                    helpText("Select which columns of your data file shall be used in
                             the event detection"),

                    # quick buttons for fast select -> actions are defined in server part
                    actionButton("csvSelectAll", "select ALL"),
                    actionButton("csvUnselectAll", "unselect All"),

                    # dynamic list for generating checkboxes
                    createCsvCheckBoxes()
                )
            ))
    })

    #--------------------------------------------------------
    observeEvent(input$csvSelectAll,{
        setAllCsvCheckBoxes(session)
    })

    observeEvent(input$csvUnselectAll,{
        resetAllCsvCheckBoxes(session)
    })
}
