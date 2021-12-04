<<<<<<< HEAD
# ST558-RProj3
Below if the report with all the packages that will be needed.

If you run the code below you will be able to view the shiny app I created to review the top 50 chess players accourding to Chess.com. Data was gathered via an API provided by Chess.com.

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

shiny::runGitHub("ST558-Project-3", "Smora0713", ref = "main", subdir = "/ST558-Project3/")
=======
# ST558-RProj3
Below if the report with all the packages that will be needed.

If you run the code below you will be able to view the shiny app I created to review the top 50 chess players accourding to Chess.com. Data was gathered via an API provided by Chess.com.

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

shiny::runGitHub("ST558-Project-3", "Smora0713", ref = "main", subdir = "/ST558-Project3/")
>>>>>>> be8583543f86da1d644b04bc18af75e850e83aa5
```