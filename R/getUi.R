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
                        menuItem("Data Preparation", tabName = "dataPreparation"),
                        menuItem("Algorithm Config", tabName = "algConfig"),
                        menuItem("Run Event Detection", tabName = "runMode"),
                        menuItem("Visualization", tabName = "visu")
            )
        ),

        # Body Contents of each tab
        dashboardBody(
            # Use Shinyjs for locking and unlocking buttons
            shinyjs::useShinyjs(),

            tabItems(
                # Objective function configuration tab
                tabItem(tabName = "dataPreparation",
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

                # Configuration Tab for spot settings
                tabItem(tabName = "algConfig",
                        fluidRow(
                            column(6,
                                   wellPanel(
                                       h4("General Settings"),
                                       uiOutput("spotConfigUI")
                                   ),
                                   wellPanel(
                                       h4("Optimizer"),
                                       uiOutput("fieldOptimizerSelector"),
                                       uiOutput("optimizerUI")
                                   )
                            ),
                            column(6,
                                   wellPanel(
                                       h4("Modelling"),
                                       uiOutput("fieldModelSelector"),
                                       uiOutput("modelUI")
                                   ),
                                   wellPanel(
                                       h4("Design Settings"),
                                       uiOutput("fieldDesignSelector"),
                                       uiOutput("designUI")
                                   )
                            )
                        )
                ),

                tabItem(tabName = "runMode",
                        fluidRow(
                            wellPanel(
                                actionButton("runCreateDOE", "Create DOE"),
                                actionButton("evaluateData","Evaluate Data/Update Model"),
                                actionButton("runSpotIter", "Run SPOT", onclick="Shiny.onInputChange('spotInterrupted',false)"),
                                actionButton("proposeNewPoint", "Propose next Point"),
                                actionButton("resetData", "Reset"),
                                actionButton("interruptSpot","Interrupt Spot", onclick="Shiny.onInputChange('spotInterrupted',true)"),
                                checkboxInput("rLogMode",label = "Log Only")
                            )
                        ),
                        fluidRow(
                            uiOutput("bestFound"),
                            column(6

                            ),
                            column(6,
                                   uiOutput("slidersResult"),
                                   uiOutput("variableSelectors"),
                                   #if (requireNamespace("plotly", quietly = TRUE)) {
                                   #       plotlyOutput("plotlyModelPlot")
                                   # },
                                   plotlyOutput("resultModelPlot")
                            )
                        )
                ),

                tabItem(tabName = "visu",
                        shiny::tags$head(shiny::tags$style(shiny::HTML(
                            "#rLog {height: 200px; overflow: auto; }"
                        ))),
                        wellPanel(
                            h4("R Recreation Log"),
                            verbatimTextOutput("rLog"),
                            checkboxInput("shortenLog", "Only Show Entrys after last data reset"),
                            uiOutput("clipButton")
                        )
                )
            )
        )
    )
}
