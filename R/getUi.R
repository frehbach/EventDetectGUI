#' Defines the shiny UI-part of the GUI
#'
#' Generates the UI part of the GUI.
#' This method is used internally in the starting process,
#' manual use of this function is not advised.
#'
#' @export
getUIPage <- function(){
    dashboardPage(
        # Theme Color Setting
        skin = "blue",
        dashboardHeader(title = "owosGUI"),

        # Creation of Main Tabs
        dashboardSidebar(
            sidebarMenu(id = "tabs",
                        menuItem("Data Selection", tabName = "dataSelection"),
                        menuItem("Data Visualization", tabName = "visu"),
                        menuItem("Algorithms Config", tabName = "algConfig"),
                        menuItem("Run Event Detection", tabName = "runMode"),
                        menuItem("Result Visualization", tabName = "resView")
            )
        ),

        # Body Contents of each tab
        dashboardBody(
            # Use Shinyjs for locking and unlocking buttons
            shinyjs::useShinyjs(),

            tabItems(
                # Objective function configuration tab
                tabItem(tabName = "dataSelection",
                        fluidRow(
                            wellPanel(
                                tags$div(title="Select a data file to import",
                                         fileInput("inputFile", "Data File Selection",
                                                   multiple = TRUE,
                                                   accept = c("text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv"))),
                                checkboxInput("csvAdvancedConfig", "Specify advanced CSV read options", FALSE),
                                conditionalPanel(
                                    condition = "input.csvAdvancedConfig == true",
                                    tags$hr(),

                                    # Input: Checkbox if CSV input file has header
                                    checkboxInput("csvUseHeader", "Header", TRUE),
                                    fluidRow(
                                        column(4,
                                        # Input: Select separator for csv interpretation
                                        radioButtons("csvSep", "Data Separator",
                                                     choices = c(Comma = ",",
                                                                 Semicolon = ";",
                                                                 Tab = "\t"),
                                                     selected = ",")),
                                        column(4,
                                        # Input: Select quotes style for csv input
                                        radioButtons("csvQuote", "How are quotes handled?",
                                                     choices = c(None = "",
                                                                 "Double Quote" = '"',
                                                                 "Single Quote" = "'"),
                                                     selected = '"'))
                                    )
                                )
                            ),
                            uiOutput("outDataSelection"),
                            DT::dataTableOutput("outDataHead")
                        )
                ),

                tabItem(tabName = "visu",
                        wellPanel(
                            fluidRow(
                                column(4,uiOutput("visuDataSlider")),
                                column(4,uiOutput("visuDataInput"))
                            ),
                            uiOutput("uiOutPlotSelectColumnsVisu")
                        ),
                        plotlyOutput("plotVisu")
                ),

                # Configuration Tab for Algorithm settings
                tabItem(tabName = "algConfig",
                        fluidRow(
                            column(6,
                                   wellPanel(
                                       h4("General Settings"),
                                       uiOutput("generalUI")
                                   ),
                                   wellPanel(
                                       h4("Preprocessor Selection"),
                                       uiOutput("preProcessSelector"),
                                       uiOutput("preProcessUI")
                                   )
                            ),
                            column(6,
                                   wellPanel(
                                       h4("Event Detection Algorithm"),
                                       uiOutput("algorithmSelector"),
                                       uiOutput("algorithmUI")
                                   ),
                                   wellPanel(
                                       h4("Postprocess Settings"),
                                       uiOutput("postProcessSelector"),
                                       uiOutput("postProcessUI")
                                   )
                            )
                        )
                ),

                tabItem(tabName = "runMode",
                        h4("Sadly not yet implemented...")
                ),

                tabItem(tabName = "resView",
                        h4("Sadly not yet implemented...")
                )
            )
        )
    )
}
