# ST558-RProj3

```
# Install/Load the packages
pkgs <- c("shiny", 
          "magrittr",
          "dplyr", 
          "lubridate",
          "jsonlite",
          "tidyverse",
          "countrycode",
          "knitr",
          "reshape2",
          "data.table",
          "DT",
          "tree"
          "caret")
install.packages(pkgs)

#Upload all the packages required
lapply(pkgs, library, character.only = TRUE)

shiny::runGitHub("ST558-RProj3", "Smora0713", ref = "main", subdir = "/ST558-Project-3/")
```