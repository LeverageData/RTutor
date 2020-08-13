# Some customization to yaml


# custom handler for booleans: only convert "TRUE" and "FALSE" to boolean
yaml.bool.handler.yes <- function(val) {
  #message(paste("bool: ", val))
  if (val=="TRUE" | val=="true")
    return(TRUE)
  return(val)
}

# custom handler for booleans: only convert "TRUE" and "FALSE" to boolean
yaml.bool.handler.no <- function(val) {
  #message(paste("bool: ", val))
  if (val=="FALSE" | val=="false")
    return(FALSE)
  return(val)
}

#' Reads a yaml file and returns as a list
#'
#' Provide either \code{file} or \code{test}. This input will be interpreted as yaml code, barring the exceptions given by the other options.
#'
#' @param file A character vector specifying a file to read from.
#' @param text A character vector to directly interpret with yaml syntax. Takes precedent over \code{file} if both are provided.
#' @param verbose Shall the input vector after modifications be shown? This vector is given to \code{load_yaml()}
#' @param keep.quotes Normally quotes are interpreted by yaml. This allows to write quotes as normal and they are handled seperately.
#' @param quote.char For internal handling quotes are replaced by \code{quote.char} and retransformed afterwards. The usage of \code{quote.char} within \code{text} or \code{file} is therefore forbidden if \code{keep.quotes} is \code{TRUE}.
#' @param colon.handling There exists the following ways to deal with colons, with \code{replace.colon.char} being the default:
#' \itemize{
#'  \item \strong{replace.colon.char}: If one wants a colon within the generated text without it being interpreted by yaml, just use this option and write \code{colon.char} everywhere where a colon should be. 
#'  \item \strong{replace.all}: This allows to use colons within the yaml text as normal, e.g. for quizzes. The first colon in each line is interpreted by yaml and all others are taken verbatim. When the line starts with - \strong{all} colons are replaced. This implies that this option breaks multi-level yaml structure, so use with care! To keep specified multi-level yaml structures, one may provide allowed code words to \code{colon.replace.exceptions}. Providing for example \emph{"question"} here allows using several quizzes with only one check button.
#'  \item \strong{none}: No replacement takes place. All colons are interpreted as yaml-colons.
#' }
#' @param colon.replace.exceptions A vector with exceptions to allow for specified multi-level-yaml structures
#' @param space.after.colon Should a space after a colon be forced? \strong{Caution:} This only applies to actually \emph{written} ":" within the text and not those provided by \code{replace.colon.char}.
#' @param colon.char For internal handling colons are replaced by \code{colon.char} and retransformed afterwards. The usage of \code{colon.char} for verbatim usage within \code{text} or \code{file} is therefore forbidden if \code{colon.handling} is anything other than \code{none}.
#' @param check.by.row Allows better error handling by singling through the lines.
#' @param utf8 If \code{TRUE} (Default) the provided character vector is forced to UTF-8 Encoding which is necessary for yaml.
#' 
#' @return A list according to the provided yaml-structure
#' @examples
#' text.char = "question: What is 20*20?
#' sc:
#'  - 100
#'  - 200
#'  - 400*
#'  - 500
#' success: Great, your answer is correct__COLON__ Super!
#' failure: Try again."
#' 
#' read.yaml(text=text.char)
#' 
#' @export
read.yaml = function(file=NULL, text=NULL, verbose=FALSE, keep.quotes=TRUE, quote.char = "__QUOTE__", colon.char = NULL,  check.by.row=FALSE,  utf8=TRUE) {
  restore.point("read.yaml")
  
  if (!is.null(file)) {
    file.str = paste0(" in ", file)
  } else {
    file.str = ""
  }
  if (is.null(text)) {
    str = suppressWarnings(paste(readLines(file, warn=FALSE), collapse = "\n"))
  } else {
    str = text
  }

  if (utf8) {
   # str = enc2utf8(str)
    Encoding(str) <- "UTF-8"
  }
  
  #Handlers
  handlers=list("bool#yes"=yaml.bool.handler.yes,"bool#no"=yaml.bool.handler.no)
  if (keep.quotes) {
    str = gsub('"',quote.char,str,fixed=TRUE)
  } 
  if(keep.quotes || !is.null(colon.char)){
    yaml.string.handler = function(val) {
      if(keep.quotes) val = gsub(quote.char,'"',val,fixed=TRUE)
      if(!is.null(colon.char)) val = gsub(colon.char,':',val,fixed=TRUE)
      return(val)
    }
    handlers[["str"]]=yaml.string.handler
  }
  
  #Output of what has been done
  if (verbose)
    cat(str)

  if (check.by.row) {
    sep.str = strsplit(str,"\n", fixed=TRUE)[[1]]
    for (row in 1:length(sep.str)) {
      cat("\n try to read rows 1:",row,"\n")
      txt = paste0(sep.str[1:row],collapse="\n")
      tryCatch(
        yaml.load(txt, handlers=handlers),
        error = function(e) {
          str = paste0(as.character(e),file.str, " row ",row,"\n")
          rows = max(row-2,1):min(row+1,length(sep.str))
          str = paste0(str,paste0(rows,": ",sep.str[rows],collapse="\n"))
          stop(str, call.=FALSE)
        }
      )
    }
  }
  
  tryCatch(
    li <- yaml.load(str, handlers=handlers),
    error = function(e) {
      str = paste0(as.character(e),file.str)
      stop(str, call.=FALSE)
    }
  )

  if (utf8) {
    li = mark_utf8(li)
  }
  
  li
  #suppressWarnings(yaml.load(str, handlers=list("bool#yes"=yaml.bool.handler.yes,"bool#no"=yaml.bool.handler.no)))

}

examples.read.yaml = function() {

  fn = paste0("D:/libraries/XEconDB/Structures/Games/LureOfAuthorityAlternative.yaml")
  obj = read.yaml(fn)
  obj$variants
}


# Prints list read from a yaml file
# @export
print.yaml = function(obj) {
  if (class(obj)=="character") {
    cat(obj)
  } else {
    cat(as.yaml(obj))
  }
}

read.yaml.blocks = function(txt, add.txt =TRUE, omit.header=FALSE, tab.width=3) {
  restore.point("read.yaml.blocks")

  first.char = substring(txt,1,1)
  start = nchar(txt)>0 & first.char!=" " & first.char!="#" & first.char !="\t"
  start = which(start)
  name = str.left.of(txt[start],":")
  end = c(start[-1]-1,length(txt))

  if (!add.txt) {
   ret = quick.df(name=name, start.row=start, end.row=end)
  } else {
    if (!omit.header) {
      block.txt = sapply(seq_along(start), function(i) {
        paste0(txt[int.seq(start[i]+omit.header,end[i])], collapse="\n")
      })
    } else {
      i = 4
      block.txt = sapply(seq_along(start), function(i) {
        space.str = paste0(rep(" ", tab.width), collapse="")
        str = txt[int.seq(start[i]+omit.header,end[i])]
        left = ifelse(str.starts.with(str,space.str), tab.width+1,1)
        str = substring(str, left)
        paste0(str, collapse="\n")
      })
    }
    ret = quick.df(name=name, start.row=start, end.row=end, txt=block.txt)
  }
  ret
}


