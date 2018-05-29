#' Runs on package attach
#'
#' The package startup message is defined here. It shall give a clue on how to
#' start the spotGUI and which suggests might want to be installed.
#'
#' @keywords internal
.onAttach <- function(libname, pkgname){
    if (!interactive()) return()
    packageStartupMessage("*** Welcome to the spotGUI package ***\n\n
                          There is only one command that you will need to use the spotGUI:\n
                          runSpotGUI()\n\n
                          ")
}
