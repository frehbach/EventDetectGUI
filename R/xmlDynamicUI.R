xmlGetRootElement <- function(){
    xmlInfo <- xmlParse(system.file("spotConfig.xml", package="spotGUI"))
    xmlList <- xmlToList(xmlInfo)
    xmlList
}


#' xmlGetAllConfiguredControlElements
#'
#' Parses the spotconfig xml file. Reads all entries for optimizers, designGenerators and modelBuilders.
#' For each configured entrie, a list with the entries name and all of its configurable parameters is returned.
#'
#' @return List with all configured optimizers, designGenerators and modelBuilders,
#' as well as their respective parameters.
#'
#' @examples
#' spotGUI:::xmlGetAllConfiguredControlElements()
xmlGetAllConfiguredControlElements <- function(){
    xmlList <- xmlGetRootElement()
    if(is.null(xmlList)){
        return(NULL)
    }
    ctrlElements <- NULL
    namesList <- names(xmlList)
    for(i in 1:length(xmlList)){
        if(namesList[i] %in% c("optimizer","model","design")){
            paramsList <- list()
            subList <- xmlList[[i]]$variableList
            for(j in 1:length(subList)){
                if(length(subList[[j]])>1){
                    paramsList <- c(paramsList, subList[[j]]$name)
                }
            }
            ctrlElements[[xmlList[[i]]$name]] <- paramsList
        }
    }
    return(ctrlElements)
}

getUIListFromVariables <- function(element, rootName){
    uiList <- list()
    elementNames <- names(element)
    vars <- which(elementNames == "variable")

    for(var in vars){
        localVariable <- element[[var]]
        uiList[[length(uiList) + 1]] <- getUiElementFromXML(localVariable, rootName)
    }
    uiList
}

getUIListFromVarLists <- function(element, rootName){
    uiList <- list()
    elementNames <- names(element)
    varLists <- which(elementNames == "variableList")
    for(l in varLists){
        localVarList <- element[[l]]

        if(!grepl("xml_",rootName,fixed = T)){
            inputID <- paste0("xml_",rootName,localVarList$name)
        }else{
            inputID <- paste0(rootName,localVarList$name)
        }


        uiList[[length(uiList) + 1]] <- checkboxInput(inputId = inputID, label = localVarList$name)

        localNames <- names(localVarList)
        if("variableList" %in% localNames){
            uiList[[length(uiList) + 1]] <- getUIListFromVarLists(localVarList,paste0(rootName,localVarList$name))
        }

        uiList[[length(uiList) + 1]] <- conditionalPanel(condition = paste0("input.xml_",rootName,localVarList$name, " == true"),
                                                         wellPanel(getUIListFromVariables(localVarList,paste0(rootName,localVarList$name))))
    }
    uiList
}

getSelectedElementList <- function(groupString, selectedInput, input){
    xmlList <- xmlGetRootElement()

    nameList <- names(xmlList)
    ids <- which(nameList == groupString)

    idOfSelectedElement <- NULL

    for(i in ids){
        #Loop through XML elements which have the given groupString
        elementName <- xmlList[[i]]$name
        if(elementName == selectedInput){
            idOfSelectedElement <- i
        }
    }

    return(xmlList[[idOfSelectedElement]])
}

getUiXML <- function(strID , input, selectedInput = NULL, selectedElement = NULL){
    if(is.null(selectedInput)){
        selectedInput <- input[[paste0(strID,"Selector")]]
    }
    if(is.null(selectedElement)){
        selectedElement <- getSelectedElementList(strID, selectedInput, input)
    }
    extraParameterInput <- list(textInput(paste0(strID,"ExtraParameters"),
                                          label = "Additional Parameterlist"))

    uiList <- list()
    uiList <- c(uiList, getUIListFromVarLists(selectedElement,selectedInput),
                getUIListFromVariables(selectedElement,selectedInput),
                extraParameterInput)
    uiList
}

getToolTipTitle <- function(xmlElement,rootName){
    if(!(grepl("control", rootName))){
        return(NULL)
    }
    methodName <- gsub("control", "", rootName)
    if(methodName == "general"){
        helpParams <- getHelpSpotControlParameters()
    }else{
        helpParams <- getAllHelpControlListParams(methodName)
    }
    toolTip <- helpParams[[xmlElement$name]]
    toolTip <- gsub("\n", "", toolTip)
    toolTip <- gsub("\t", "", toolTip)
    return(toolTip)
}

getUiElementFromXML <- function(xmlElement,rootName){
    varType <- xmlElement$type
    tooltipTitle <- getToolTipTitle(xmlElement,rootName)
    shinyBS::tipify(switch(varType,
           "integer" = numericInput(paste0("xml_",rootName,xmlElement$name),label = xmlElement$name, value = xmlElement$default,
                                    min = if(!is.null(xmlElement$min)){xmlElement$min}else{NA},
                                    max = if(!is.null(xmlElement$max)){xmlElement$max}else{NA}, step = 1),
           "numeric" = numericInput(paste0("xml_",rootName,xmlElement$name),label = xmlElement$name, value = xmlElement$default,
                                    min = if(!is.null(xmlElement$min)){xmlElement$min}else{NA},
                                    max = if(!is.null(xmlElement$max)){xmlElement$max}else{NA}),
           "string" = textInput(paste0("xml_",rootName,xmlElement$name),label = xmlElement$name,
                                value = if(!is.null(xmlElement$default)){xmlElement$default}else{""}),
           "closure" = textInput(paste0("xml_",rootName,xmlElement$name),label = xmlElement$name,
                                value = if(!is.null(xmlElement$default)){xmlElement$default}else{""}),
           "boolean" = checkboxInput(paste0("xml_",rootName,xmlElement$name),label = xmlElement$name,
                                     value = if(!is.null(xmlElement$default)){eval(parse(text = xmlElement$default))}else{T})
    ), title = tooltipTitle)
}

getUiSelectorXML <- function(strID, input, returnUIElement = T){
    xmlList <- xmlGetRootElement()

    nameList <- names(xmlList)
    ids <- which(nameList == strID)

    inputSelectorNames <- list()

    variableInputs <- list()

    for(i in ids){
        #Loop through XML elements which have the given strID
        element <- xmlList[[i]]
        elementNames <- names(element)
        inputSelectorNames[[length(inputSelectorNames) + 1]] <- element$name
    }

    if(returnUIElement){
        selectInput(paste0(strID,"Selector"), "Selection",
                    choices = inputSelectorNames)
    }else{
        inputSelectorNames
    }
}

getObjectiveFunctionSelectorXML <- function(input){
    funList <- getUiSelectorXML(strID = "objectiveFunction",input = input,returnUIElement = F)

    list(
        selectInput("objectiveFunction", "Objective function:",
                list("SPOT-Internal Functions" = funList
                                ,"External Functions" =
                                list("Function from R-Environment" = "rEnv")
                     ,"No Objective Function" = list("Manual Input" = "mInput"))),
        conditionalPanel(condition = "input.objectiveFunction == 'rEnv'"
                         , textInput("funName","Name of Function in Environment"))

    )
}

generateInputUI <- function(input,initVariables,configInitiated){
    assign("inputDimensions", NULL, envir=spotGuiEnv)
    if((input$objectiveFunction == "rEnv") | (input$objectiveFunction == "mInput")){
        selectedElement <- NULL
    }else{
        selectedElement <- getSelectedElementList(groupString = "objectiveFunction",
                                                  selectedInput = input[["objectiveFunction"]],input = input)
    }

    amountOfGeneratedElements <- 0
    ##Generate UI element for each dimension
    if(length(selectedElement) >=3){
        for(i in 3:length(selectedElement)){
            insertUI(
                selector = '#objectiveFunctionInputParameters',
                where = "beforeEnd",
                ui = createDimensionElement(input,selectedElement[[i]], i-2,initVariables,configInitiated))
            amountOfGeneratedElements <- amountOfGeneratedElements + 1
        }
    }
    return(amountOfGeneratedElements)
}

createDimensionElement <- function(input,listElement, index, initVariables, configInitiated){
    if(is.null(listElement)){
        listElement <- list("name" = "", "type" = "numeric",
                            "lower"="0","upper"="1","amount"="1")
    }

    observerRemDim <- observeEvent(input[[paste0("removeDimension",as.character(index))]],{
        removeUI(selector = paste0("#inputPanel", index), immediate = T)
        isolate({
            x <- get("inputDimensions",envir=spotGuiEnv)
            assign("inputDimensions", x[x!=index], envir=spotGuiEnv)
            })
        initVariables(configInitiated())})

    observerChangeDim <- observeEvent(input[[paste0("dimensionAmount",as.character(index))]],{
        initVariables(configInitiated())
    })

    colList <- list(selectInput(inputId = paste0("dimensionType",as.character(index)),
                                label = "Type",choices = list("numeric","integer","factor"),
                                selected = listElement$type),
                    numericInput(paste0("lowerBound",as.character(index)),
                                 label = "lower bound", value = listElement$lower),
                    numericInput(paste0("upperBound",as.character(index)),
                                 label = "upper bound", value = listElement$upper),
                    numericInput(paste0("dimensionAmount",as.character(index)),
                                 label = "amnt Dimensions", value = listElement$amount),
                    actionButton(inputId = paste0("removeDimension",as.character(index)),
                                 label = "",icon = icon("minus-circle"), style = "margin-top: 25px"),
                    observerRemDim,
                    observerChangeDim
                    )

    element <- wellPanel(id = paste0("inputPanel",index), fluidRow(
        column(width = 2, colList[[1]]),
        column(width = 3, colList[[2]]),
        column(width = 3, colList[[3]]),
        column(width = 3, colList[[4]]),
        column(width = 1, colList[[5]])))
    isolate(assign("inputDimensions", c(get("inputDimensions",envir=spotGuiEnv),index), envir=spotGuiEnv))
    return(element)
}
