# #< quiz
# parts:
#   - question: What is 20*20?
#     choices:
#         - 100
#         - 200
#         - 400*
#         - 500
#     multiple: FALSE
#     success: Great, your answer is correct!
#     failure: Try again.
#   - question: State pi up to 2 digits
#     answer: 3.14
#     roundto: 0.01
# award:
#   title: Quiz master
#   text: You solved the quiz!
# #>

answer.quiz = function(name, ps = get.ps()) {
  restore.point("answer.quiz")
  
  qu = ps$rps$addons[[paste0("addon__quiz__", name)]]
  if (is.null(qu)) {
    message(paste0("Cannot find quiz ", name,". Check the problem set at least once before calling this function."))
    return(invisible())
  }
  
  if (length(qu$parts)>1) {
    stop("Sorry, multi part quizzes cannot yet be solved inside RStudio.")
  }
  part = qu$parts[[1]]
  
  if (part$type == "sc") {
    msg = paste0("Type the answer's number (1-",length(part$choices),") and then press Enter: ")
  } else if (part$type != "mc") {
    msg = paste0("Type the answer and then press Enter: ")
  } else {
    stop("Multiple choice quizzes with more than one solution are not yet supported in the RStudio environment.")
  }
  # Give focus to console
  try(rstudioapi::sendToConsole("", execute=FALSE, echo=TRUE),silent = TRUE)
  
  # Collect answer
  answer <- readline(prompt=msg)
  
  
  if (part$type =="numeric") {
    answer = as.numeric(answer)
    correct = is.true(abs(answer-part$answer)<part$roundto)
  } else if (part$type == "sc"){
    answer = as.numeric(answer)
    if (is.na(answer)) {
      cat("\nYou have to enter the number of the correct answer.")
      return(invisible())
    }
    correct = isTRUE(answer==part$correct.choices)
  } else {
    correct = setequal(answer,part$answer)
  }
  
  if (correct) {
    ans.msg = part$success
    ans.msg = gsub("<p><font color='black'>","",ans.msg, fixed=TRUE)
    ans.msg = gsub("</font></p>","",ans.msg, fixed=TRUE)
    cat(paste0("\n",ans.msg))
  } else {
    ans.msg = part$failure
    ans.msg = gsub("<p><font color='red'>","",ans.msg, fixed=TRUE)
    ans.msg = gsub("</font></p>","",ans.msg, fixed=TRUE)
    message(paste0(ans.msg))
  }
  
  # Update state
  qu$state$part.solved[1] = correct
  qu$state$solved = all(qu$state$part.solved)
  
  
  rta = qu$rta; state=qu$state
  rta$solved = state$solved
  rta$points = (sum(state$part.solved) / length(state$part.solved))*rta$max.points
  
  # Save ups with correctly answered quiz
  process.checked.addon(rta,from.shiny=FALSE)
  return(invisible())
}

rtutor.addon.quiz = function() {
  list(
    package = "RTutor",
    type = "quiz",
    mode = "block",
    parse.fun = rtutor.quiz.block.parse,
    shiny.init.fun = rtutor.quiz.init.shiny,
    shiny.ui.fun = rtutor.quiz.shiny.ui,
    task.txt.fun = rtutor.quiz.task.txt.fun,
    sol.txt.fun = rtutor.quiz.sol.txt.fun,
    out.txt.fun = rtutor.quiz.sol.txt.fun
  )
}

rtutor.quiz.task.txt.fun = function(ao,solved=FALSE,...) {
  quiz.md(ao,solution = solved)
} 

rtutor.quiz.sol.txt.fun = function(ao,solved=TRUE,...) {
  quiz.md(ao,solution = solved)
}

rtutor.quiz.shiny.ui = function(ao, ...) {
  quiz.ui(ao)  
}

rtutor.quiz.init.shiny = function(ao,ps=get.ps(), app=getApp(),...) {
  add.quiz.handlers(qu=ao, quiz.handler=rtutor.quiz.handler)    
}

rtutor.quiz.block.parse = function(txt,type="quiz",name="",id=paste0("addon__",type,"__",name),args=NULL,...) {
  restore.point("rtutor.quiz.block.parse")
  qu = shinyQuiz(id = id,yaml = merge.lines(txt),add.handler = FALSE)

  rta = as.environment(list(
    id=id,type=type,optional=TRUE, changes.env=FALSE, max.points=qu$max.points,
    solved=FALSE, points=0, was.solved=FALSE, had.points=0
  ))
  qu$rta = rta
  qu
}

rtutor.quiz.handler = function(app,qu,part.ind, part.solved, solved,...) {
  restore.point("rtutor.quiz.handler")
  
  rta = qu$rta; state=qu$state
  
  rta$solved = state$solved
  rta$points = (sum(state$part.solved) / length(state$part.solved))*rta$max.points
  process.checked.addon(rta)
}

examples.quiz = function() {
    yaml = '
parts:
  - question: What is 20*20?
    choices:
        - 100
        - 200
        - 400*
        - 500
    multiple: FALSE
    success: Great, your answer is correct!
    failure: Try again.
  - question: State pi up to 2 digits
    answer: 3.14
    roundto: 0.01
award:
  title: Quiz master
  text: You solved the quiz!

  '
  app = eventsApp()

  qu = parse.quiz.yaml(yaml)
  qu$ui = quiz.ui(qu)
  app$ui = qu$ui
  add.quiz.handlers(qu)
  
  runEventsApp(app, launch.browser=rstudioapi::viewer)
  
}

quizDefaults = function(lang="en") {
  if (lang=="de") {
    list(
      success = "Richtig!",
      failure= "Leider noch nicht richtig.",
      success_color = "black",
      failure_color = "red",
      points_txt = "Punkte",
      point_txt = "Punkt"
    )
  } else {
    list(
      success = "Great, you answered correctly!",
      failure= "Sorry, not yet correct.",
      success_color = "black",
      failure_color = "red",
      points_txt = "points",
      point_txt = "point"
    )
  }
}

# Create a shiny quiz widget
#
# @param id the id of the quiz
# @param qu a list that contains the quiz fields as would have
#        been parsed by read.yaml from package YamlObjects
# @param yaml alternatively to qu, is yaml a string that specifies the quiz
# @param quiz.handler a function that will be called if the quiz is checked.
#        The boolean argument solved is TRUE if the quiz was solved
#        and otherwise FALSE
shinyQuiz = function(id=paste0("quiz_",sample.int(10e10,1)),qu=NULL, yaml,  quiz.handler=NULL, add.handler=TRUE, single.check.btn=TRUE, defaults=quizDefaults(lang=lang), lang="en") {
  restore.point("shinyQuiz")

  if (is.null(qu)) {
    yaml = enc2utf8(yaml)
    yaml.prepared = try(prepare.yaml.quiz(yaml,colon.char="__COLON__", colon.replace.exceptions=c("question","choice_commentary_single","choice_expr", "choice_text")))
    if (is(yaml.prepared,"try-error")) {
      err = paste0("When preprocessing and transforming choice_comments of quiz:\n",paste0(yaml, collapse="\n"),"\n\n",as.character(yaml.prepared))
      stop(err,call. = FALSE)
    }
    yaml.read = try(read.yaml(text=yaml.prepared, colon.char = "__COLON__"), silent=TRUE)
    if (is(yaml.read,"try-error")) {
      err = paste0("When importing quiz:\n",paste0(yaml.prepared, collapse="\n"),"\n\n",as.character(yaml.read))
      stop(err,call. = FALSE)
    }
    qu = try(aftercare.yaml.quiz(yaml.read), silent=TRUE)
    if (is(qu,"try-error")) {
      err = paste0("When post processing quiz:\n",paste0(yaml.read, collapse="\n"),"\n\n",as.character(qu))
      stop(err,call. = FALSE)
    }
  }

  if (is.null(qu[["id"]])) {
    qu$id = id
  }
  if (is.null(qu$parts)) {
    qu$parts = list(qu)
  }


  qu$single.check.btn = single.check.btn
  if (qu$single.check.btn) {
     qu$checkBtnId = paste0(qu$id,"__checkBtn")
  }

  qu$parts = lapply(seq_along(qu$parts), function(ind) init.quiz.part(qu$parts[[ind]],ind,qu))
  np = length(qu$parts)
  
  qu$max.points = sum(sapply(qu$parts, function(part) part[["points"]]))
  
  qu$state = as.environment(list(part.solved=rep(FALSE,np), solved=FALSE))

  qu$ui = quiz.ui(qu)

  if (add.handler)
    add.quiz.handlers(qu, quiz.handler)
  qu
}

init.quiz.part = function(part=qu$parts[[part.ind]], part.ind=1, qu, has.check.btn=!qu$single.check.btn, defaults=quizDefaults()) {
  restore.point("init.quiz.part")

  part = copy.into.missing.fields(dest=part, source=defaults)

  if (!is.null(part[["sc"]])) {
    part$choices = part$sc
    part$multiple = FALSE
    #part$type = "sc"
  } else if (!is.null(part[["mc"]])) {
    part$choices = part$mc
    part$multiple = TRUE
    #part$type = "mc"
  }


  if (!is.null(part$choices)) {
    correct.choices = which(str.ends.with(part$choices,"*"))
    if (is.null(part$multiple)) {
      part$multiple = length(correct.choices) != 1
    }
    part$correct.choices = correct.choices
    part$choices[correct.choices] = str.remove.ends(part$choices[correct.choices],right=1)
    part$answer = unlist(part$choices[correct.choices])
    names(part$choices) =NULL
    if (part$multiple) {
      part$type = "mc"
    } else {
      part$type = "sc"
    }
  } else if (!is.null(part$answer)) {
    if (is.numeric(part$answer)) {
      part$type = "numeric"
      if (is.null(part$roundto)) part$roundto=1e-7
    } else {
      part$type = "text"
    }
  } else {
    stop(paste0("The quiz with question ", part$question, " has neither defined the field 'answer' nor the field 'choices'."))
  }

  if (is.null(part[["points"]])) {
    part$points = 1
  }

  txt = part$success
  
  if (part$points==1) {
    txt = paste0(txt," (", part$points, " ", defaults$point_txt,")")
  } else if (part$points > 0 ) {
    txt = paste0(txt," (", part$points, " ", defaults$points_txt,")")
  }
  txt = colored.html(txt, part$success_color)
  part$success =  markdownToHTML(text=txt,encoding = "UTF-8", fragment.only=TRUE)

  txt = colored.html(part$failure, part$failure_color)
  part$failure =  markdownToHTML(text=txt,encoding = "UTF-8", fragment.only=TRUE)

  part$id = paste0(qu$id,"__part", part.ind)
  part$answerId = paste0(part$id,"__answer")
  if (has.check.btn) {
    part$checkBtnId = paste0(part$id,"__checkBtn")
  } else {
    part$checkBtnId = NULL
  }
  part$resultId = paste0(part$id,"__resultUI")
  part$ui = quiz.part.ui(part)
  part$solved = FALSE

  if (is.null(part$points)) {
    part$points = 1
  }
  
  part
}

quiz.ui = function(qu, solution=FALSE) {
  restore.point("quiz.ui")
  pli = lapply(seq_along(qu$parts), function(i) {
    part = qu$parts[[i]]
    if (i < length(qu$parts)) {
      hr = hr()
    } else {
      hr = NULL
    }

    if (solution) {
      if (is.null(part$sol.ui)) {
        part$sol.ui = quiz.part.ui(part, solution=TRUE)
      }
      return(list(part$sol.ui,hr))
    } else {
      return(list(part$ui,hr))
    }
  })
  if (!is.null(qu$checkBtnId)) {
    pli = c(pli, list(actionButton(qu$checkBtnId,label = "check")))
  }

  pli
}

quiz.part.ui = function(part, solution=FALSE, add.button=!is.null(part$checkBtnId)) {
  restore.point("quiz.part.ui")
  
  #R Markdown insists on starting numbered lists with 1. This is undesired behavior, especially given multi-part quizzes where one might want to label them and md2html doesn't know about the other questions. 
  if(stringr::str_detect(part$question,pattern="^([:space:]*)(\\d+)\\.(.*)")){
    quest.split = stringr::str_match(part$question,pattern="^([:space:]*\\d+)\\.(.*)")[-1]
    part$question = stringr::str_c(quest.split[1],"\\.",quest.split[2])
  }
  
  question = markdownToHTML(text=part$question, fragment.only=TRUE,encoding="UTF-8")
  #question = rmdtools::md2html(part$question)
  
  # Allow RMD Formatting (e.g. bolding and MathJax) with single choice and multiple choice
  if(part$type %in% c("sc","mc")){
    choices = lapply(transform.save.html(part$choices),HTML)
    answer = lapply(transform.save.html(part$answer),HTML)
  }
  
  head = list(
    HTML(paste0("<p>",question,"</p>"))
  )
  if (solution) {
    if (part$type=="numeric") {
      answer = textInput(part$answerId, label = "",value = part$answer)
    } else if (part$type =="text") {
      answer = textInput(part$answerId, label = "",value = part$answer)
    } else if (part$type=="mc") {
      answer = checkboxGroupInput(part$answerId, "",choiceNames=choices,choiceValues = as.list(1:length(choices)),selected = answer)
    } else if (part$type=="sc") {
      answer = radioButtons(part$answerId, "",choiceNames=choices,choiceValues = as.list(1:length(choices)), selected=answer)
    }
  } else {
    if (part$type=="numeric") {
      answer = textInput(part$answerId, label = "",value = "")
    } else if (part$type =="text") {
      answer = textInput(part$answerId, label = "",value = "")
    } else if (part$type=="mc") {
      answer = checkboxGroupInput(part$answerId, "",choiceNames=choices,choiceValues = as.list(1:length(choices)))
    } else if (part$type=="sc") {
      answer = radioButtons(part$answerId, "",choiceNames=choices,choiceValues = as.list(1:length(choices)), selected=NA)
    }
  }

  if (add.button) {
    button = actionButton(part$checkBtnId,label = "check")
  } else {
    button = NULL
  }
  list(head,answer,uiOutput(part$resultId),button)
}

quiz.md = function(qu, solution=FALSE, add.numbers=FALSE) {
  restore.point("quiz.md")
  li = lapply(seq_along(qu$parts), function(i) {
    part = qu$parts[[i]]
    quiz.part.md(part, solution=solution, add.numbers=add.numbers)
  })
  paste0(li, collapse="\n")
}


quiz.part.md = function(part, solution=FALSE, add.numbers=FALSE) {
  restore.point("quiz.part.md")
  
  #Add Quiz: if not already there
  question_start = stringr::str_sub(part$question,1,7)
  if(stringr::str_detect(question_start,"Quiz:")){
    head = stringr::str_c("\n",part$question,"\n")
  } else {
    head = stringr::str_c("\n**Quiz:** ",part$question,"\n")
  }
  if (solution) {
    if (part$type=="numeric" | part$type == "text") {
      answer = paste0("Answer: ", part$answer)
    } else if (part$type=="mc" | part$type=="sc") {
      ans = part$choices
      mark = rep("[ ]", length(ans))
      mark[ans %in% part$answer] =  "[x]"
      answer = paste0("- ", ans, " ", mark,"\n", collapse="\n")
    }
  } else {
    if (part$type=="numeric" | part$type == "text") {
      answer = "Answer: "
    } else if (part$type=="mc" | part$type=="sc") {
      ans = part$choices
      if (add.numbers) {
        answer = paste0("[", seq_along(ans),"]: ", ans, collapse="\n")
      } else {
        answer = paste0("- ", ans, " [   ]\n", collapse="\n")
      }
    }
  }
  paste0(head,"\n", answer)
}

add.quiz.handlers = function(qu, quiz.handler=NULL, id=qu$id){
  restore.point("add.quiz.handlers")
  app = getApp()
  if (is.null(app)) {
    cat("\nCannot add quiz handlers since no shinyEvents app object is set.")
    return()
  }

  if (!qu$single.check.btn) {
    for (part.ind in seq_along(qu$parts)) {
      part = qu$parts[[part.ind]]
      buttonHandler(part$checkBtnId,fun = click.check.quiz, part.ind=part.ind, qu=qu, quiz.handler=quiz.handler)
    }
  } else {
    buttonHandler(qu$checkBtnId,fun = click.check.quiz, part.ind=0, qu=qu, quiz.handler=quiz.handler)
  }
}

click.check.quiz = function(app=getApp(), part.ind, qu, quiz.handler=NULL, ...) {
  restore.point("click.check.quiz")

  # check all parts
  if (part.ind == 0) {
    for (part.ind in seq_along(qu$parts))
      click.check.quiz(app=app, part.ind=part.ind,qu=qu, quiz.handler=NULL)

    if (!is.null(quiz.handler)) {
      quiz.handler(app=app, qu=qu, part.ind=0, part.correct=NA, solved=qu$state$solved)
    }
    return(qu$state$solved)
  }

  part = qu$parts[[part.ind]]
  answer = getInputValue(part$answerId)
  restore.point("click.check.quiz.inner")

  choice.commentary.text = NULL
  if (part$type =="numeric") {
    answer = as.numeric(answer)
    correct = is.true(abs(answer-part$answer)<part$roundto)
  } else if (part$type == "text"){
    correct = setequal(answer,part$answer)
  } else if (part$type %in% c("sc","mc")){
    correct = setequal(answer, part$correct.choices)
    if(!correct){
      choice.commentary.text = generate.choice.commentary(as.numeric(answer), part)
    }
  } else {
    stop(stringr::str_c("Invalid part$type provided in click.check.quiz. Tried: ",part$type, " but only numeric, text, sc and mc are allowed."))
  }
  if (correct) {
    cat("Correct!")
    setUI(part$resultId,withMathJax(HTML(part$success)))
  } else {
    wrong.output = stringr::str_c(part$failure,colored.html(choice.commentary.text,color=part$failure_color),sep = "\n")
    cat("Wrong")
    setUI(part$resultId,withMathJax(HTML(wrong.output)))
  }
  qu$state$part.solved[part.ind] = correct
  qu$state$solved = all(qu$state$part.solved)

  if (!is.null(quiz.handler)) {
    quiz.handler(app=app, qu=qu, part.ind=part.ind, part.correct=correct, solved=qu$state$solved)
  }

}

generate.choice.commentary = function(chosen, part){
  restore.point("generate.choice.commentary")
  
  #only if commentary exists
  if(is.null(part$choice_commentary)){
    return(NULL)
  }
  
  pos.commentary = part$choice_commentary
  commentary = lapply(pos.commentary, FUN=function(x){
    expr.str = x[[1]][[1]]$choice_expr
    text = x[[1]][[2]]$choice_text
    is.valid = try(eval(parse(text=expr.str)))
    if (is(is.valid,"try-error")) {
      err = paste0("When evaluating quiz:\n",paste0(part$answerId),"\n","the following expression could not be evaluated:",expr.str,"\n")
      stop(err,call. = FALSE)
    }
    if(is.valid){
      return(text)
    } else {
      return(NULL)
    }
  }) %>% unlist() %>% stringr::str_c(collapse="<br>")
  
  return(rmdtools::md2html(transform.save.html(commentary)))
}

prepare.yaml.quiz = function(str, colon.char = "__COLON__", colon.replace.exceptions=c()){
  restore.point("prepare.yaml.quiz")

    #To have a controlled environment we do not allow blank lines. 
  str.split = stringr::str_split(str, "\n")[[1]]
  str.blank = str.split %>% sapply(FUN=stringr::str_length, USE.NAMES = FALSE) == 0
  str.defacto.blank = stringr::str_detect(str.split, "^[:blank:]+$")
  str.split = str.split[!str.blank & !str.defacto.blank]  
  
  #There might be multiple questions, so we want to deal with them separately.
  questions = stringr::str_detect(str.split,pattern="^([:space:]|-)*question:")
  question.ind = questions %>%
    as_tibble() %>%
    transmute(ind=cumsum(value)) %>%
    unlist(use.names=FALSE)
  
  #to have compatibilty with lists-numeration in the case we do not start with a question (e.g. due to parts:)
  if(question.ind[1]==0){
    question.ind = question.ind+1 
  }
  
  question.tbl = tibble(lines=str.split, question.ind=question.ind) %>%
    group_by(question.ind)
  
  #We can now work within each question within the grouping.
  question.tbl.choice.ident = question.tbl %>%
    mutate(is.choice.com = stringr::str_detect(lines,pattern="^([:space:]|-)*choice_commentary\\{.+\\}:"))
  
  #Prepare yaml
  if(any(question.tbl.choice.ident$is.choice.com)){
    
    #Without choice commentary base
    question.tbl.wo.choice = question.tbl.choice.ident %>%
      filter(!is.choice.com) %>%
      dplyr::select(-is.choice.com)
    question.tbl.li = question.tbl.wo.choice %>%
      group_by(question.ind) %>%
      group_split() %>%
      lapply(FUN=function(x){x %>% dplyr::select(-question.ind)})
    
    #extract expression and text
    question.tbl.choice = question.tbl.choice.ident %>%
      filter(is.choice.com) %>%
      dplyr::select(-is.choice.com) %>%
      mutate(regex = as.data.frame(stringr::str_match(lines,pattern="(^([:space:]*-*[:space:]*)choice_commentary\\{)(.+)(\\}:)[:space:]*(.*)"))[,c(3,4,6)]) %>%
      mutate(choice.trail=if_else(is.na(regex[,1]),"",regex[,1]),choice.expr = regex[,2], choice.text = regex[,3]) %>%
      dplyr::select(-regex)
    
    #Shorthand: If there is only a single number or ! and a single number, we want to change the expression
    question.tbl.choice = question.tbl.choice %>%
      mutate(shorthand=stringr::str_detect(choice.expr,"^\\!?[:digit:]+$")) %>%
      mutate(shorthand_neg=shorthand&stringr::str_detect(choice.expr,"\\!")) %>%
      mutate(shorthand_pos=shorthand&!shorthand_neg) %>%
      mutate(choice.expr = if_else(shorthand_pos,stringr::str_c("c(",choice.expr,") %in% chosen"),choice.expr)) %>%
      mutate(choice.expr = if_else(shorthand_neg,stringr::str_c("!(c(",stringr::str_extract(choice.expr,"[:digit:]+"),") %in% chosen)"),choice.expr)) %>%
      dplyr::select(-shorthand, -shorthand_neg, -shorthand_pos)
    
    
    #Colons make problems -> change to MACRO
    question.tbl.choice = question.tbl.choice %>%
      mutate(choice.expr=stringr::str_replace_all(choice.expr,":",colon.char)) %>%
      mutate(choice.text=stringr::str_replace_all(choice.text,":",colon.char))
    
    #out of this generate a commentary yaml
    commentary.yaml = list()
    for(i in 1:length(question.tbl.li)){
      my.quest = question.tbl.choice %>% filter(question.ind == i)
      
      if(nrow(my.quest)==0){ #not a question with commentary
        commentary.yaml[[i]] = NULL
        next
      }
      
      #sanity check:
      if(length(unique(my.quest$choice.trail))!=1){
        warning(stringr::str_c("The leading white spaces of choice_commentary differ. This might lead to a wrong interpretation."))
      }
      my.trailing = my.quest$choice.trail[1]
      
      start = stringr::str_c(my.trailing,"choice_commentary:",collapse="")
      li = lapply(1:nrow(my.quest),FUN=function(x){
        line = my.quest[x,]
        line.c = c( stringr::str_c(my.trailing,"    - ",stringr::str_c("choice_commentary_single:",collapse="")),
              stringr::str_c(my.trailing,"          - ",stringr::str_c("choice_expr: ",line$choice.expr,collapse="")),
              stringr::str_c(my.trailing,"          - ",stringr::str_c("choice_text: ",line$choice.text,collapse=""))
        )
      }) %>% unlist()
      commentary.yaml[[i]] = tibble(lines=c(start,li))
    }
    
    final.tbl.li = list()
    
    
    #Combine them again
    for(i in 1:length(question.tbl.li)){
      if(length(commentary.yaml)>=i){ #R automatically shortens lists and doesn't like trailing NULLs
        final.tbl.li[[i]] = bind_rows(question.tbl.li[[i]],commentary.yaml[[i]])
      } else {
        final.tbl.li[[i]] = question.tbl.li[[i]]
      }
    }
    str.split = bind_rows(final.tbl.li) %>%
      unlist() %>%
      unname()
  }  
  
  #Now everything is in neat yaml form even though seperated. However there might be yaml functionalities which are not wanted!
  
  # Convert tabs to spaces
  str.split = gsub("\t","   ",str.split)
  
  #Handle colons
  str.split.trim = str.split %>%
    stringr::str_trim(side="left") #ignore leadinge white spaces
  
  no.leading.white = str.split %>%
    stringr::str_length() - stringr::str_length(str.split.trim) #Number of leading white spaces to be added again
  
  str.split.colon.safe = sapply(str.split.trim,FUN=function(x){
    if(stringr::str_length(x)==0){
      return(x)
    } #noting to do
    if(stringr::str_sub(x,start=1,end=1)=="-"){ #choices of quiz, new questions (if multiple) or choice commentaries -> keep colons, except in the case of exceptions
      colons = stringr::str_locate_all(x,":")[[1]][,"end"]
      keep.colons = stringr::str_locate_all(x,stringr::str_c(stringr::str_c(colon.replace.exceptions,":"),collapse="|"))[[1]][,"end"]
      transform.colons = colons[!(colons %in% keep.colons)]
      if(length(transform.colons)==0){
        return(x)
      }
      #Go from the back to not mess up the indizes
        for(i in length(transform.colons):1){
        stringr::str_sub(x,transform.colons[i],transform.colons[i]) = colon.char
        }
      return(x)
    }
    #here the first colon is important for yaml, but the others are replaced
    #if only one colon, everything is ok
    if(stringr::str_count(x,":")<=1){
      return(x)
    }
    #otherwise replace all after the first one
    str.split.split = str_split(x,":")[[1]]
    str.split.first.colon = c(stringr::str_c(str.split.split[1:2],collapse=":"),str.split.split[-(1:2)])
    str.split.colon.char = stringr::str_c(str.split.first.colon,collapse=colon.char)
    return(str.split.colon.char)
  }, USE.NAMES = FALSE)
  
  #Now only correct colons are still left, so me make these pretty
  str.split.colon.safe = gsub(":",": ",str.split.colon.safe)
  str.split.colon.safe = gsub(":  ",": ",str.split.colon.safe)
  
  #Now set everything in quotes
  str.split.syntax.safe = sapply(str.split.colon.safe,FUN=function(x){
    #There are two types of expected lines: those with "(- )? <command>: <Text>?*", and "- <Text>"
    with.colon = stringr::str_detect(x, ": ")
    if(with.colon){
      meaningful.text = stringr::str_detect(x, "^.*: +[^[:blank:]]+")
      if(!meaningful.text){
        return(x)
      }
      sep.strings = stringr::str_match(x,"^(.*): +(.*)$")[c(2,3)]
      safe.string = stringr::str_c(sep.strings[1],": \"",sep.strings[2],"\"")
    } else {
      text.to.quote = stringr::str_match(x,"^- +(.*)$")[c(2)]
      safe.string = stringr::str_c("- \"",text.to.quote,"\"")
    }
    return(safe.string)
  }, USE.NAMES=FALSE)
  
  #combine again for further processing
  missing.white.spaces = lapply(no.leading.white,FUN=function(x){stringr::str_c(rep(" ",x),collapse="")})
  str.split.syntax.safe = stringr::str_c(missing.white.spaces,str.split.syntax.safe)
  final.str = stringr::str_c(str.split.syntax.safe, collapse="\n")
  
  final.str
}

aftercare.yaml.quiz = function(yaml.quiz){
  restore.point("aftercare.yaml.quiz")
  if(is.list(yaml.quiz)){
    lapply(yaml.quiz,aftercare.yaml.quiz)
  } else if (length(yaml.quiz)>1){
    sapply(yaml.quiz,aftercare.yaml.quiz,USE.NAMES=FALSE)
  } else {  
    item = yaml.quiz
    if(stringr::str_length(item)>2){
      item.nice = stringr::str_sub(item,2,-2)
    } else {
      item.nice = item 
    }
    return(type.convert(item.nice, as.is=TRUE))
  }
}
