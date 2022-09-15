# Update section ===============================================================
## Update checkboxinput based on selected rows in table=========================
checkboxUpdateBySelectedRows <- function(inputid, selected, new_selected, input_table, session){
  if(!isTruthy(new_selected) & !isTruthy(selected)){
    x <- character(0)
    updateCheckboxGroupInput(session, inputid,
                             "0 node(s) selected:",
                             choices = x,
                             selected = x)
  }
  if(length(new_selected)!=0){
      x.name = input_table$term[match(unique(c(selected, new_selected)), input_table$id)]
      x.name <- paste0(": ", x.name)
      x.name[x.name == ": NA"] <- ""
      updateCheckboxGroupInput(session, inputid,
                               label = paste(length(unique(c(selected, new_selected))), "node(s) selected:"),
                               choiceValues = unique(c(selected, new_selected)),
                               choiceNames = paste0(unique(c(selected, new_selected)), x.name),
                               selected = unique(c(selected, new_selected))
      )
  }
}

## Update checkboxinput if a new node is added by clicking======================
checkboxUpdateByAddButton <- function(inputid, centers, node_now,
                                      edge_matrix_full, dict.combine,
                                      session){
  
  if(length(centers)!=0 & length(node_now)!=0){
    if(!(node_now %in% centers)){
      x = c(centers, node_now)
      x.name = dict.combine$term[match(x, dict.combine$id)]
      updateCheckboxGroupInput(session, inputid,
                               label = paste(length(x), "node(s) selected:"),
                               choiceValues = x,
                               choiceNames = paste0(x,": ",x.name),
                               selected = x
      )
    }
  }
}
