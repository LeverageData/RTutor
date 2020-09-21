# RTutor (main)

## 2020-09-18 [Only LD Fork]

* Intrododuced highlight boxes. They are highly customizable and allow to highlight important parts of the text - and are fully RMD compatible.
* Quizzes are now bold in the output file. If the quiz is already starting with Quiz or **Quiz, then we refrain from adding another Quiz: Indicator.
* Fixed Inconsistcency in the display of quizzes between shiny and knitr. Now the html input in questions is seperated by newlines. 
* Not having a newline before Lists no longer breaks the outputfile. Also intendation works as intendet in Lists. This fixes different interpretations in RMD between the generation of the shiny problemset and the knitr interpreter of the outputfile. This also included deleting all tabs in html tables.
* Fixed Bug where "info" boxes of the same name would overwrite earlier ones.

## 2020-09-16 

* Display Message if no user name was specified in the context of stats.

## 2020-08-30 [Only LD Fork]
* Now having a problemset without chunks and multiple exercises no longer throws as warning
* Exercise buttons are now shown for all exercises except the last independent of chunks
* It now possible to change the text and style of the exercise button.

## 2020-08-13 [Only LD Fork]

* YAML Functionality further limited in quizzes
* It is now possible to have a *negative* shorthand in quizzes.

## 2020-08-11

* More convenient adaptive hints using `hint.stud.call` and `hint.stud.assign` and also more flexible combination of custom and auto hints using `auto.hint()` and `auto.hint.else()`.

## 2020-08-07 [Only LD Fork]

* Quizzes have now added flexibility like choice commentaries, RMD and HTML support

## 2020-06-10

* Allow option for shiny based problem sets that one can start editing any chunk and previous 

## 2019-07-31

* Allow adaptive custom hints.

* Make automatic tests and hints more informative for some common errors. Also automatic hints for %>% chains are now considerably more informative.

* Add test arguments check.cols and sort.cols for simple testing only selected columns of a computed data frame.

*  Created the companion package RTutorSAGI that helps to analyse
submission logs in order to identify parts where students systematically
get stuck.

## 2019-04-10

Add simple functionality to render data frames as word table if
output solution is knitted as Word file

## 2019-03-23

Add fill_in block to easier handle syntactially incorrect R code
that has placeholders. Use it when you want to show students something like

filter(df, a == ___)

where students have to replace the ___.

## 2019-02-19

Several changes in the last 4 years, e.g.:

- optional chunks
- more robust hints
- automatically try to run previous chunks in shiny mode
- possibility to download submission file from shiny


## 2015-11-29 preknit and show in noeval mode

- When creating the problem set, we can now already knit for each
  chunk the sample solution.
- A preknitted shiny based problem set can be shown with the parameter
  `noeval=TRUE`. Then user code is then only parsed but not evaluated.
  Tests and hints only compare the parsed expression with the parsed
  sample solution. The noeval mode can be useful for hosting problem sets
  on an own webserver, where evaluation of user code is a high 
  security risk or the user shall have no direct access to the data.
  The cost is less flexibility in testing the user code,
  e.g. in noeval mode,  `x=1+2` will not be seen as correct if the
  sample solution is `x=2+1`.

## 2015-11-17 print data.frames as html table in shiny-based RTutor

- In the shiny interface of RTutor data.frames are now printed by
  default as HTML tables (and only a maximum number of rows are shown).
  The printing behavior can be modified on a global level in the call
  to show.ps or on the chunk level. See `DataFramesExample_sol.Rmd`
  for examples.

## 2015-11-13: hints at beginning of chunk possible

- You can now add a hint block at the beginning of a chunk,
  before any other command.
  While other hints are linked to the previous expression that will
  be tested, the initial hint works for the whole chunk.
  It will be shown in addition to any expression specific hint.
  It will also  be shown if the code cannot be evaluated
  or in a chunk that has no expression to be tested.


## 2015-11-08: htmlwidget support

- Allow chunks that output htmlwidgets. You need to set the chunk option `output="htmlwidget"`, and specify the widget type in the chunk option `widget`, e.g. `widget="leaflet"`. An example is given in `LeafletExample_sol.Rmd` in the `./inst/examples` folder.

## 2015-11-02: quiz blocks

- Added quiz blocks. You need to provide the argument `addons="quiz"` in create.ps, and add `#< quiz quiz_name ... #>` blocks. For an example, see `QuizExample_sol.Rmd` in the `./inst/examples` folder.


## 2015-11-01: option for memoisation when reading data

- Added the argument `use.memoise` in create.ps. If set to `TRUE`, the functions listed in the argument `memoise.funs` will be memoised when running and showing a problem set. By default memoisation is done for a set of functions that read data from files. This saves time and memory if the same data set is repeatedly loaded in different 
exercises.

## 2015-06-01: awards also in web-based problem sets

- awards can now be used and will be correctly displayed also in web-based problem sets

## 2015-04-01: optional code chunks and notes with chunks

- Add chunk option `optional=TRUE`. Optional chunks don't need to be solved in an exercise and their computations will not be available in subsequent chunks.

- We can now put RTutor chunks and texts in notes using the lines
```  
  #! start_note "Note Title"
   ...
  #! end_note
```
  Unlike info blocks the chunks inside a note can be solved.    Chunks inside a note should have the chunk option `optional=TRUE`.
