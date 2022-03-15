

colorpickerUI <- function(id, color) {
  ns <- NS(id)
  colourpicker::colourInput(
    ns("colorpicker"), id, color,
    returnName = TRUE, 
    palette = "limited",
    allowedCols = colors)
}

colorpickerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
   ns <- NS(id)
   input$colorpicker
})}


# allowedCols = toupper(c("#ea5545","#f46a9b","#ef9b20","#edbf33","#ede15b","#bdcf32","#87bc45","#27aeef",
#                 "#b33dc6","#e60049","#0bb4ff","#50e991","#e6d800","#9b19f5","#ffa300","#dc0ab4",
#                 "#b3d4ff","#00bfa0","#b30000","#7c1158","#4421af","#1a53ff","#0d88e6","#00b7c7",
#                 "#5ad45a","#8be04e","#ebdc78","#fd7f6f","#7eb0d5","#b2e061","#bd7ebe","#ffb55a",
#                 "#ffee65","#beb9db","#fdcce5","#8bd3c7", "#FEDE00"
#                 ))
# 
# colored_group <- list("Disease_Codified" = "#FEDE00",
#                       "Drug_Codified" = "#F46A9B",
#                       "Lab_Codified" = "#8BE04E",
#                       "Procedure_Codified" = "#27AEEF",
#                       "Disease_NLP" = "#FEDE00",
#                       "Drug_NLP" = "#F46A9B",
#                       "Lab_NLP" = "#8BE04E",
#                       "Procedure_NLP" = "#27AEEF",
#                       "ACTI" = "#EF9B20",
#                       "LIVB" = "#BDCF32",
#                       "PHEN" = "#B33DC6",
#                       "PHYS" = "#E60049",
#                       "PROC" = "#9B19F5",
#                       "CHEM" = "#00BFA0")



