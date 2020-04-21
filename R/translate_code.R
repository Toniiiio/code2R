library(rstudioapi)
library(stringr)
library(magrittr)
library(glue)

# Making R welcoming for "foreign programmers"!
# By no means an attempt to be complete nor to be a full code translator.
# An interactive cheat sheet integrated in RStudio IDE.
# How do you write that in R?

gsub



translate_code <- function() {
  context <<- rstudioapi::getSourceEditorContext()

  rng <<- context$selection[[1]]$range
  txt <<- context$selection[[1]]$text
  print(rng)
  if(!nchar(txt)){
    message("No text was selected, attempting to match with selected line instead.")
    line <- context$contents[rng$start[1]]
    txt <- line
    message(glue::glue("The line reads: '{line}'."))
  }



  r_py_dict <- data.frame(
    python = "os.listdir",
    r = "list.files",
    stringsAsFactors = FALSE
  )

  r_py_dict <- rbind(
    r_py_dict,
    c("os.getcwd", "getwd"),
    c("replace", "gsub"),
    c("count", "table"),
    c("%", "%%"),
    c("[[].*?[]]", "list(.*?)"),
    c("strip", "trimws"),
    c("np.where", "which",
      "[-2:]", "substring")
  )

  py_code <- txt %>% trimws()

  print(py_code)

  #stringr::str_extract_all(py_code, "[.*?]")
  #stringr::str_match(string = py_code, pattern = "\\[(.*?)\\]")[, 2]

  idx <- sapply(r_py_dict$python, grepl, x = py_code, perl = TRUE) %>% which

  idx <- which(py_code == r_py_dict$python)

  r_code <- r_py_dict$r[idx]
  print(r_code)

  if (!length(r_code)) {
    warning("no match found!")
    return()
  }

  replacem <- gsub(
    pattern = py_code,
    replacement = r_code,
    x = txt
  )

  rstudioapi::modifyRange(
    location = rng,
    text = replacem
  )

  rstudioapi::sendToConsole(
    code = paste0("?", r_code),
    execute = TRUE
  )
}



### MATLAB
# library(pdftools)
# x <- pdf_text("https://cran.r-project.org/doc/contrib/Hiebeler-matlabR.pdf")
# x[4] %>% strsplit(split = "\n")

# Code2R - Rstudio Addin to turn your "primary program. language" (command) into R commands.

# Last week i was working on a dataset, that could also have value for my data science colleagues.
# As i wanted to make the generation process reproducible and my colleagues are all Python users,
# i decided to leave R beside for once.

# While i know most R commands by heart, i struggle remembering some of them for Python.
# To save time in future and for the community i decided to build a dictionary (R command + corresp. Python command).

# Finally, i remembered about the great rstudioapi from RStudio and decided to make an interactive addin:

# GIF



# R+Python (not a battle)
# Collaboration @ Union Investment
# Making R welcoming for "foreign programmers"
# DataScience

# Github link

# Next steps:
# The project just started this weekend, so there is still a lot to do:
# - Enrichen the dictionary
# - Add new languages (Javascript, Scala, etc.)
# - Integration for Visual Studio Code, Spyder, etc.
# - enabling the reverse "foreign language" to R.
