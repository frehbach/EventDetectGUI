# Returns the a list with upper and lower bounds that the user set in the objective function
# configuration page
getBounds <- function(input){
    lbounds <- NULL
    ubounds <- NULL
    types <- NULL

    dims <- get("inputDimensions",envir=spotGuiEnv)

    for(e in dims){
        if(is.na(as.integer(input[[paste0("dimensionAmount",e)]]))){
            return(list(NA,NA,NA))
        }
        lbounds <- c(lbounds, rep(as.numeric(input[[paste0("lowerBound",e)]]),
                                  times = as.integer(input[[paste0("dimensionAmount",e)]])))
        ubounds <- c(ubounds, rep(as.numeric(input[[paste0("upperBound",e)]]),
                                  times = as.integer(input[[paste0("dimensionAmount",e)]])))
        types <- c(types, rep(input[[paste0("dimensionType",e)]],
                              times = as.integer(input[[paste0("dimensionAmount",e)]])))
    }
    return(list(lbounds,ubounds,types))
}

getNDim <- function(input){
    return(length(getBounds(input)[[1]]))
}

# For higher dimensional plots variable selection + sliders are used.
# This method returns all variables which are not selected to be directly in the plot
# Thus it returns those variables which are controlled via sliders
getNotSelectedVariables <- function(input, mode){
    notSelectedVars <- NULL

    req(input[[paste("selectorX",mode,sep="")]])

    for(i in 1:getNDim(input)){
        if((!input[[paste("selectorX",mode,sep="")]] == paste("X",i,sep=""))
           & (!input[[paste("selectorY",mode,sep="")]] == paste("X",i,sep=""))){
            notSelectedVars <- c(notSelectedVars,i)
        }
    }

    notSelectedVars
}

# Returns vector with all slider values on a page defined by mode
getPlotSliderValues <- function(input, mode){
    sliders <- getNotSelectedVariables(input, mode)
    sliderValues <- NULL
    for(s in sliders){
        sliderValues <- c(sliderValues, input[[paste("slider",mode,"x", s, sep="")]])
    }
    sliderValues
}

# returns the user selected objective function
getObjectiveFunction <- function(input, asText = F){
    fun <- as.character(input$objectiveFunction)
    if(fun == "rEnv"){
        fun <- input$funName
    }
    if(!asText){
        fun <- get(fun)
    }
    fun
}

# returns the user selected model
getModel <- function(input){
    as.character(input$modelSelector)
}

getOptimizer <- function(input){
    as.character(input$optimizerSelector)
}

getDesignGenerator <- function(input){
    as.character(input$designSelector)
}

getClosureVariableFromUI <- function(input, inputName, asText = F){
    if((input[[inputName]] == "NULL")){
        if(asText){
            return("NULL")
        }else{
            return(NULL)
        }
    }else if((input[[inputName]] == "NA")){
        if(asText){
            return("NA")
        }else{
            return(NA)
        }
    }else if(is.numeric(input[[inputName]])){
        return(as.numeric(input[[inputName]]))
    }else{
        if(!asText){
            tryCatch(return(get(input[[inputName]])),
                     error=return(eval(parse(text=input[[inputName]]))))
        }else{
            return(input[[inputName]])
        }
    }
}

# Returns dynamically created radio buttons for plot axis selection
getPlotSelectorButtonList <- function(input, mode){
    uiOut <- NULL
    if(getNDim(input) > 2){
        uiOut <- fluidRow(
            column(6,
                   radioButtons(inputId=paste("selectorX",mode,sep=""), label="X-Axis Variable",
                                choices=c(unlist(lapply(1:getNDim(input), function(i) {
                                    paste("X",i,sep="")}))))
            ),
            column(6,
                   radioButtons(inputId=paste("selectorY",mode,sep=""), label="Y-Axis Variable",
                                choices=c(unlist(lapply(1:getNDim(input), function(i) {
                                    paste("X",i,sep="")}))),selected = "X2")
            )
        )
    }
    uiOut
}

# Returns dynamically created slider list, fot plot value selection
getPlotSliderList <- function(input,mode){
    if(getNDim(input) > 2){
        b <- getBounds(input)
        lb <- b[[1]]
        ub <- b[[2]]

        notSelectedVariables <- getNotSelectedVariables(input,mode)

        sliders <- lapply(notSelectedVariables, function(i) {
            displayName <- paste("x",i,sep="")
            inputName <- paste("slider",mode,"x",i,sep="")
            sliderInput(inputName, displayName, min=lb[i], max=ub[i], value=lb[i], step = ((ub[i]-lb[i])/100))
        })
    }else{
        sliders <- list()
    }
    sliders
}

getXMLVariableUI <- function(input, rootName, var, asText = F){
    uiName <- paste0(rootName,var$name)
    l <- list()
    if(var$type == "string" & asText){
        l[[var$name]] <- paste0("\"",input[[uiName]],"\"")
    }else if(var$type == "closure"){
        l[[var$name]] <- getClosureVariableFromUI(input,uiName,asText)
    }else if(var$type == "numeric"){
        num <- as.numeric(input[[uiName]])
        if(is.na(num)){
            if(asText){
                num <- "NULL"
            }else{
                num <- NULL
            }
        }
        l[[var$name]] <- num
    }else{
        l[[var$name]] <- input[[uiName]]
    }
    if(!(length(l) == 0)){
        names(l) <- var$name
    }
    l
}

getXMLVarListUI <- function(input, rootName, varList, asText = F){
    rootName <- paste0(rootName, varList$name)
    indVars <- which(names(varList) == "variable")
    indVarLists <- which(names(varList) == "variableList")

    l <- list()
    for(ind in indVarLists){
        l<- c(l,getXMLVarListUI(input, rootName, input))
    }

    for(var in indVars){
        l <- c(l,getXMLVariableUI(input,rootName,varList[[var]],asText))
    }
    l
}

getExtraParametersList <- function(input, strID, asText = F){
    inputName <- paste0(strID,"ExtraParameters")
    if(input[[inputName]] == ""){
        return(NULL)
    }else{
        return(getClosureVariableFromUI(input, inputName, asText))
    }
}

getControlList <- function(input, strID, asText = F){
    rootName <- "xml_"
    xmlRoot <- xmlGetRootElement()
    xmlFilteredForID <- xmlRoot[which(names(xmlRoot)==strID)]
    if(strID == "general"){
        selected <- "general"
    }else{
        selected <- input[[paste0(strID,"Selector")]]
    }
    rootName <- paste0(rootName,selected)
    indexSelected <- NULL
    for(i in 1:length(xmlFilteredForID)){
        element <- xmlFilteredForID[[i]]
        if(element$name == selected){
            indexSelected <- i
        }
    }
    settingsSelectedElement <- xmlFilteredForID[[indexSelected]]$variableList
    resList <- getXMLVarListUI(input,rootName,settingsSelectedElement,asText)
    resList$types <- getBounds(input)[[3]]
    resList <- c(resList, getExtraParametersList(input,strID,asText))

    return(resList)
}

getSpotControl <- function(input, asText = F){
    extraParams <- getControlList(input, "general", asText)
    extraParams$types <- getBounds(input)[[3]]
    l <- list(
         model = getModel(input),
         modelControl = getControlList(input,"model", asText),
         optimizer = getOptimizer(input),
         optimizerControl = getControlList(input,"optimizer", asText),
         design = getDesignGenerator(input),
         designControl = getControlList(input,"design", asText)
    )

    l <- c(l, extraParams)
    if(!asText){
        l$optimizer <- get(l$optimizer)
        l$model <- get(l$model)
        l$design <- get(l$design)
    }
    return(l)
}
