find.funs = function(call, max.level=Inf, level=1) {
  if (level > max.level) return(NULL)
  if (!is.call(call)) return(NULL)
  fun.name = as.character(call[1])
  sub.names = lapply(call[-1], function(e1) {
    find.funs(e1, max.level=max.level, level=level+1)
  })
  names = unique(c(fun.name,unlist(sub.names, use.names=FALSE)))
  names
}

#' Returns file path of current file
#'
#' When called out of RStudio API returns the current path of the active file without the file name.
#'
#' @return A character value defining the current path.
#' @examples
#' if(rstudioapi::isAvailable()) get.current.file.path()
#' 
#' @export
get.current.file.path = function(){
  if(rstudioapi::isAvailable()){
    complete.path = rstudioapi::getActiveDocumentContext()$path
  } else {
    stop("RStudio not running.")  
  }
  complete.path.splitted = stringr::str_split(complete.path, pattern="/")[[1]]
  file.path.splitted = complete.path.splitted[-length(complete.path.splitted)]
  file.path = stringr::str_c(file.path.splitted,collapse="/")
  file.path
}

#' Sets Working directory to current directory
#'
#' When called out of RStudio API or per Source sets the working directory to the current directory. If called from console nothing happens.
#'
#' @return -
#' 
#' @export
set.wd.current = function(){
  if(rstudioapi::isAvailable()){
    rstudio.path = get.current.file.path()
  } else {
    rstudio.path = NULL
  }
  
  if(!is.null(rstudio.path) && rstudio.path!=""){
    setwd(rstudio.path)
  } else {
    source.path = parent.frame(2)$ofile
    if(!is.null(source.path)){
      setwd(source.path)
    }
  }
}
