colors<-c("#66C2A5","#F8C998","#FFD700","#EE82EE","#98F898","#8DA0CB","#A898F8","#82B9FD",
          "#FBD51C","#D700FE","#00FDD7","#FD0D2E","#FC844B","#E9DBF8","#0DA7FB","#85FA00",
          "#FF0DC1","#C7ED9B","#D2A27F","#FF96EB","#D82668","#45E3FD","#0D9600","#7AB9A8",
          "#B9ADFC","#FD8C9D","#63495A","#D2EF22","#AD70FB","#26FD8D","#B800B9","#8D880D",
          "#0047BB","#972A16","#D292AC","#006581","#85224D","#32511C","#6D2A95","#352EFE")

colorpickerUI <- function(id, color) {
  ns <- NS(id)
  colourpicker::colourInput(
    ns("colorpicker"), id, color,
    # palette = "limited",
    # allowedCols = colors,
    returnName = TRUE)
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



