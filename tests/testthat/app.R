#Main Shiny Application Call
require("spotGUI")
shinyApp(ui = getUIPage, server = getServer)
