#Note: the label and primary key of the nodes must FIRST be constrained for this function to work
#Example: addConstraint(graph, "vendor", "DUNSNumber")
add_or_update_nodes <- function(df, label, id, props = NULL, verbose = FALSE, na.act = F) {
  nodes <- list()
  # cannot have name of NA value
  miss <- !is.na(df[, id])
  uniq <- unique(df[miss, c(id, props), drop = FALSE])
  
  #initiate a counter for the true number of nodes added to the database
  j <- 1
  for (i in 1:nrow(uniq)) {
    arg <- as.list(uniq[i, c(id, props), drop = FALSE])
    arg <- arg[!is.na(arg)]
    
    #Try to create a node. If it already exists, try to upate its properties...probably need to rewrite this to only raeact to a specific error
    tryCatch({n <- do_call(createNode, quote(graph), label, arg); if (verbose) cat(j, 'Created ', label, ': ', n[[1]], '\n'); j <- j+ 1},
             
             #Properties are updated when an error arises because of a pre-existing node
             error = function(e){
               if(verbose) cat("Node ", paste(uniq[i, id]), "already exists and is uniquely constrained; attempting to update its properties \n")
               
               #Query the database to obtain the object we want to update
               query <- paste("MATCH (a:", label, "{", id, ": '", uniq[i, id], "'}) RETURN a", sep = "")
               object <- getSingleNode(graph, query)
               
               #Check to see if there are any properties; if so, update the existing node
               if(length(arg > 1)){                              
                 do_call(updateProp, quote(object), arg[2:length(arg)])
                 if(verbose) cat("updated properties \n")
               }
               else{
                 if(verbose) cat("No properties to update \n")
               }
               
             }
    )
    
    #Update the list of nodes to be returned by querying the database (may be a more efficient way to do this)
    query <- paste("MATCH (a:", label, "{", id, ": '", uniq[i, id], "'}) RETURN a", sep = "")
    object <- getSingleNode(graph, query)
    
    tryCatch(nodes[[uniq[i, id]]] <- object, error = function(e){
      cat("We got an error returning nodes: ", conditionMessage(e), i, paste(uniq[i, id]), "\n")
      readline("Press <return to continue")
    }
    )
    
  }
  nodes
}



#Note: this only currently works if entities have exactly one label
add_or_update_rels <- function(df, from, f_name, to, t_name, how, props = NULL, verbose = T, f_alias = f_name, t_alias = t_name) {
  rels <- list()
  df <- df[, c(f_name, t_name, props)]
  df <- na.omit(df)
  for ( i in 1:nrow(df) ) {
    query = paste()
    x1 <- from[[df[i, f_name]]]
    x2 <- to[[df[i, t_name]]]
    arg <- as.list(df[i, props, drop = FALSE])
    arg <- arg[!is.na(arg)]
    
    #Check to see if the relationship already exists
    query <- paste("MATCH (a:", getLabel(x1), "{", f_alias, ": '", x1[[f_alias]], "'}) -[r:", how, "] -> (b:", getLabel(x2), "{", t_alias, ": '", x2[[t_alias]], "'}) RETURN r", sep = "")
    object <- getSingleRel(graph, query)
    
    #If it doesn't exist, create the relationship
    if(is.null(object)){
      n <- do_call(createRel, quote(x1), how, quote(x2), arg)
      
      if (verbose) cat(i, 'Created rel: ', names(x1), how, names(x2), '\n')
      rels[[as.character(i)]] <- n
    }
    
    #if it does exist, try to update the properties
    else{
      if (verbose) cat("relationship for", df[i, f_name], "to", df[i, t_name], "already exists \n")
      if(length(arg > 1)){
        do_call(updateProp, quote(object), arg[2:length(arg)])
        if(verbose) cat("updated relationship properties \n")
      }
      else{
        if(verbose) cat("no properties to update \n")
      }
    }
    
  }
}
