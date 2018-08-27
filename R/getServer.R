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
        req(getEnvData("csvData"))
        cols <- getCSVCols(input)
        output$outDataHead <- DT::renderDataTable(getEnvData("csvData")[,cols],
                                                  options = list(scrollX = TRUE,
                                                                pageLength = length(input$outDataHead_rows_current),
                                                                lengthMenu = c(3, 5, 10, 15, 20)))
    }, ignoreNULL = FALSE)

    output$outDataSelection <- renderUI({
        req(input$inputFile)
        req(getEnvData("csvData"))
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

    output$visuDataSlider <- renderUI({
        req(input$outDataHead_rows_all)
        sliderInput("amntPointsVisuPlots",label = "Show last x data points", value = 100,min = 10, max = length(input$outDataHead_rows_all))
    })

    output$visuDataInput <- renderUI({
        req(input$outDataHead_rows_all)
        numericInput("amntPointsVisuInputField", label = "",100)
    })

    output$uiOutPlotSelectColumnsVisu <- renderUI({
        req(input$outDataHead_rows_all)
        selectInput('plotSelectionVisu', 'Select Plot Columns', colnames(getEnvData("csvData")[,-1]), multiple=TRUE, selectize=TRUE)
    })

    #--------------------------------------------------------
    output$plotVisu <- renderPlotly({
        if(is.null(getEnvData("csvData"))){
            return(NULL)
        }
        plotCols <- which(colnames(getEnvData("csvData")) %in% input$plotSelectionVisu)
        if(length(plotCols) < 1){
            return(NULL)
        }

        csvdata_tail <- getEnvData("csvData")[(length(input$outDataHead_rows_all)
                                               -input$amntPointsVisuInputField
                                               +1):length(input$outDataHead_rows_all),]
        csvdata_tail[[1]] <- as.POSIXlt(csvdata_tail[,1]
                                       ,tryFormats = c("%m/%d/%Y %H:%M:%OS"))
        p <- plot_ly(
            x = ~(nrow(csvdata_tail):1),
            y = ~csvdata_tail[,plotCols[1]],
            type = 'scatter',
            name = colnames(csvdata_tail)[plotCols[1]],
            mode = "lines+markers"
        ) %>% layout(
            yaxis = list(title = colnames(csvdata_tail)[plotCols[1]]),
            xaxis = list(title="t minus nth data point", autorange="reversed")
        ) %>%
           add_trace(x = ~(nrow(csvdata_tail):1) , y= c(mean(csvdata_tail[,plotCols[1]]))
                     , mode = "lines", name = "mean") %>%
            add_trace(x = ~(nrow(csvdata_tail):1) , y= c(mean(csvdata_tail[,plotCols[1]])+sd(csvdata_tail[,plotCols[1]]))
                      , mode = "lines", name = "+sigma")%>%
            add_trace(x = ~(nrow(csvdata_tail):1) , y= c(mean(csvdata_tail[,plotCols[1]])-sd(csvdata_tail[,plotCols[1]]))
                      , mode = "lines", name = "-sigma")
        if(length(plotCols) == 2){
            p <- p %>%
                add_lines(x = ~(nrow(csvdata_tail):1) , y = csvdata_tail[,plotCols[2]],
                          mode = "lines+markers", name = colnames(csvdata_tail)[plotCols[2]]
                          ,yaxis = paste0("y",2)) %>%
                layout(yaxis2 = list(overlaying = "y", side = "right"
                                     , title = colnames(csvdata_tail)[plotCols[2]]))
        }
        if(length(plotCols) > 2){
            return(NULL)
        }
        p$elementId <- NULL
        p
    })

    observeEvent(input$amntPointsVisuInputField,{
        isolate(updateSliderInput(
            session = session,
            inputId = "amntPointsVisuPlots",
            value = input$amntPointsVisuInputField
        ))
    })

    observeEvent(input$amntPointsVisuPlots,{
        isolate(updateNumericInput(
            session = session,
            inputId = "amntPointsVisuInputField",
            value = input$amntPointsVisuPlots
        ))
    })

    observeEvent(input$csvSelectAll,{
        setAllCsvCheckBoxes(session)
    })

    observeEvent(input$csvUnselectAll,{
        resetAllCsvCheckBoxes(session)
    })

    observeEvent(input$refreshVisuPlots,{

    })

    output$preProcessSelector <- renderUI({
        getUiSelectorXML("preProcess",input)
    })

    output$algorithmSelector <- renderUI({
        getUiSelectorXML("algorithm",input)
    })

    output$postProcessSelector <- renderUI({
        getUiSelectorXML("postProcess",input)
    })

    output$preProcessUI <- renderUI({
        req(input$preProcessSelector)
        getUiXML("preProcess",input)
    })

    output$algorithmUI <- renderUI({
        req(input$algorithmSelector)
        getUiXML("algorithm",input)
    })

    output$postProcessUI <- renderUI({
        req(input$postProcessSelector)
        getUiXML("postProcess",input)
    })

    output$generalUI <- renderUI({
        getUiXML("general",input, selectedInput = "general"
                 , selectedElement = getSelectedElementList("general", "general", input))
    })

    observeEvent(input$runEDS,{
        if(is.null(input[["xml_ForecastETSShow"]])){
            showModal(modalDialog(title="Load Error",
                                  "Config was not fully loaded, please revisit Config tab"))
            return()
        }
        if(!checkInputCorrectness(input)){
            return()
        }
        ctrl <- getControlList(input, "preProcess")
        return()
        tryCatch(expr = {
            detectEvents(stationBData[1:1000,-1])
        }, error = function(cond) {
            showModal(modalDialog(title="Configuration Error",HTML(paste("There seems to be an error in your configuration.<br>
                                                                         detectEvents was not able to run.<br>
                                                                         Please check for typos/misconfigurations
                                                                         in the Config Tab<br><br>Original error was:<br>",cond))
                                  ,footer=NULL,easyClose=T))
            return()
        })
    })
}
