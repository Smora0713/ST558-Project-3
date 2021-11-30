#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
setwd("C:/Users/14154/Documents/School/NCSU/STAT 558/R project 3/ST558-Project-3")
library(shiny)
library(shinydashboard)
library(magrittr)
library(dplyr)
library(lubridate)
library(jsonlite)
library(tidyverse)
library(countrycode)
library(knitr)
library(reshape2)
library(data.table)
library(DT)

referrence_table <- fromJSON(
  "https://api.chess.com/pub/leaderboards"
)

top50 <- referrence_table$daily

stats_apis_list <- as.list((paste0(top50$'@id',"/stats")))

list_of_stats <- lapply(stats_apis_list,fromJSON)

col.names <- colnames(data.frame(list_of_stats[[1]]$chess_daily)) # list of the column names since the empty dataframe wont have this by default

col.num <- ncol(data.frame(list_of_stats[[1]]$chess_daily)) # number of columns needed for our empty data frame

row.num <- nrow(top50) # number of rows, this might be repetative since we know it should always be 50 but just in case something goes wrong

all_stats <- data.frame(matrix(ncol = col.num, nrow = row.num)) # list of column list

all_fide <- data.frame(matrix(ncol = 1, nrow = row.num)) # list of column list

for (i in 1:row.num){
  #all_stats[i,] <- merge(data.frame(list_of_stats[[i]]$chess_daily),data.frame(list_of_stats[[i]]$fide))
  all_stats[i,] <- data.frame(list_of_stats[[i]]$chess_daily)
  #all_fide[i,] <- data.frame(list_of_stats[[i]]$fide,is.na = TRUE)
  #all_stats[i,] <- data.frame(list_of_stats[[i]]$fide)
}


colnames(all_stats) <- col.names

all_stats <- cbind(top50[c("player_id","username","rank","country","title","status")],all_stats) # Binding the data based on the position.

country_codes <- read.csv("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv")

country_codes <- country_codes %>% distinct(Alpha.2.code, .keep_all = TRUE) %>% mutate(country_code = gsub(" ","",Alpha.2.code)) %>% dplyr::select(country_code,Country,Latitude..average.,Longitude..average.)

new_countries <- data.frame(c("XE","XS"),c("Europe","Serbia"),c(54.5260,44.0000),c(15.2551,21.0000))
names(new_countries) <- names(country_codes)

country_codes <- rbind(country_codes,new_countries)


all_stats$country <- substr(all_stats$country,nchar(all_stats$country)-1,nchar(all_stats$country))


all_stats <- left_join(all_stats,country_codes,by = c("country" = "country_code")) %>% mutate(minutes_between_moves = (record.time_per_move/60))

set.seed(123)
world <- map_data("world")

all_stats <-all_stats %>% mutate(Latitude..average.jit = jitter(all_stats$Latitude..average., amount = 3),Longitude..average.jit = jitter(all_stats$Longitude..average., amount = 3))

all_stats_summary_WLD <- all_stats %>% dplyr::select(username,record.win,record.loss,record.draw,rank)

all_stats_summary_WLD <- gather(all_stats_summary_WLD,key = "record", value = "WLD",record.win,record.loss,record.draw) %>% arrange(username)

all_stats_summary_rating <- all_stats %>% dplyr::select(username,last.rating,best.rating,rank)

all_stats_summary_rating <- gather(all_stats_summary_rating,key = "record", value = "Rating",last.rating,best.rating) %>% arrange(username)

all_stats_summary_rating_user <- all_stats_summary_rating %>% filter(username == user)


# INSTRUCTIONS:

# An About page. The page should
##  Describe the purpose of the app
##  Briefly discuss the data and its source - providing a link to more information about the data
##  Tell the user the purpose of each tab (page) of the app
##  Include a picture related to the data (for instance, if the data was about the world wildlife
##                                         fund, you might include a picture of their logo)

# A Data Exploration page. This should allow the user to
##  Create numerical and graphical summaries
##  Change the type of plot and type of summary reported
##  Change the variables and filter the rows to change the data in the plots/summaries


# A Modeling page. You will fit three supervised learning models. Depending on your response you'll fit a multiple linear regression or generalized linear regression model, regression or classification tree, and a random forest model. This page should have three tabs to it.

##  Modeling Info tab: You should explain these three modeling approaches, the benefits of each, and the drawbacks of each. You should include some type of math type in the explanation (you'll need to include mathJax).

##  Model Fitting tab:
###  You'll split your data into a training and test set. Give the user the ability to choose the proportion of data used in each.
###  The user should have functionality for choosing model settings for each model. For all models, they should be able to select the variables used. Cross validation should be used for selecting models where appropriate.
###  When the user is ready they should be able to press a button and fit all three models on the training data.
###  Fit statistics (such as RMSE) on the training data should be reported for each model along with appropriate summaries about the model (for instance, summary() run on your lm() or glm() fit, a plot showing the variable importance from the random forest model, etc.).
###  The models should be compared on the test set and appropriate fit statistics reported.
##  Prediction tab: You should give the user a way to use one of the models for prediction. That is, they should be able to select the values of the predictors and obtain a prediction for the response.

# A Data page. The user should be able to
## ∗ Scroll through the data set
## ∗ Subset this data set (rows and columns)
## ∗ Save the (possibly subsetted) data as a file (.csv is fine but whatever you’d like)


shinyUI(fluidPage(navbarPage(title = "Tabs",

#About page
## Side panel links and description
  tabPanel("About", fluid = TRUE,
           sidebarLayout(
             sidebarPanel(
               h2("ST558 Project 3"),
               br(),
               h3("Tabs Desciption:"),
               tags$ul(
                 tags$li(strong("About:"),"Desciption of the app as a whole"),
                 tags$li(strong("Data Exploration:"),"Descriptive vizuals to get a good grasp of the data. This will all be descriptive analytics."),
                 tags$li(strong("Modeling:"),"Models to help us understand which variables in our data will help us predict the ranks."),
                 tags$li(strong("Data:"),"Raw dataset where our vizualization and models come from."),
               )
             ),

## Main panel page, descirving the app.
            mainPanel(
              h1("Top 50 Chess.com Players"),
              h3("By: Sergio Mora"),
              br(),
              h2("Purpose of the app"),
              p("This app will help us understand who the top 50 chess players from Chess.com are. Chess.com has many ways to play the game of chess but we will focus on just a few:"),
              br(),
              tags$ul(
                tags$li(p(strong("Daily:"),"These are games that are played with atleast one day between turns. Meaning a player has hours/days to think of their next move.")),
              tags$li(p(strong("daily960:"),"These are games that are played with atleast one day between turns, however the layout of the board is different. This means that standard opening and lines go out the window.")),
              tags$li(p(strong("live_rapid:"),"A 'Rapid chess' game is one where either all the moves must be completed in a fixed time of more than 10 minutes but less than 60 minutes for each player. Some rapid games also allow for time increment per move played. e.g. a 10 minute game could have an increase of 5 seconds for each move. Meaning if one players plays quickly they could go over 10 minutes alloted to them.")),
              tags$li(p(strong("live_blitz:"),"Blitz games (AKA fast chess) is where each player has less than 10 minutes to play the whole game. Some blitz games allow for time incraments similar to rapid games. This means that players have to focus both on the game and on the clock if they want to win. These sort of games allow for little time to think so often you will see players play well known openings and well known lines in the beginning to save time. However mistakes are still made even by world class players.")),
              tags$li(p(strong("live_bullet:"),"Bullet games (sometimes called 'blood games') is likely the most aggresive way to play chess. Each player has 1 minute to play the whole game with no time incraments per move. Meaning that the whole came can only last at most 2 minutes. With so little time a lot of mistakes can be made so players depend heavily on strong openings and well studies lines. Bullet games are often described as games where you have to study your opponent prior to the game."))
              ),
              br(),
              h2("Data Source"),
              p("This data comes to us from", a(href ="https://www.chess.com/" ,"Chess.com"), "which is the indesputed leader of online chess. Everyone from beginner to Grand Masters play on this platform. This specific data is being sources from", a(href ="https://www.chess.com/news/view/published-data-api#pubapi-leaderboards","Chess.com API's."), "This dataset is built from multiple API's:"),
              br(),
              tags$ul(
                tags$li(p(strong("https://api.chess.com/pub/leaderboards:"),"This API collects the top 50 players in chess.com in each type of play.")),
                tags$li(p(strong("https://api.chess.com/pub/player/{playerID}:"),"This API collects data on the specific players in the top 50. This API is dependent on the previous API.")),
              ),
              br(),
              h2("Purpose of the app"),
              p("This app is meant to get a better understanding of who the top players are and we will try to predict their rank based on their title, fide, score, last chess.com rating, and other variables from our dataset. We will also try our hand at building different models with the same data to test the effeciency of each type of model.")
            )
           )),


  tabPanel("Data Exploration", fluid = TRUE,
           sidebarLayout(
             sidebarPanel(
               h2("Exploratory data analysis"),
               radioButtons("EDAType","EDA Type",choiceValues = c("map", "bar", "hist"),choiceNames = c("Map","Bar Plot","Histogram")),
               conditionalPanel("input.EDAType == 'bar'", 
                                radioButtons("WLD_Rating","WLD or Rating",c("Win, Loss, Draw","Rating")),
                                selectInput("User","User",c(all_stats_summary_WLD$username))
                                ),
               conditionalPanel("input.EDAType == 'hist'", 
                                radioButtons("Rating","Last, Best, or Time",c("Last","Best","Time"))
                                ),
               h3("Summary Table"),
               checkboxGroupInput("stat","Statistical Summary",c("Mean" = "Means","Median" = "Medians","Standard Deviation" = "SDs"), selected = c("Means","Medians","SDs"),inline = TRUE),
               checkboxGroupInput("Values","Values",c("Best Rating" = "Best Rating","Last Rating" = "Last Rating","Minutes Between Moves"= "Minutes Between Moves"),selected = c("Best Rating","Last Rating","Minutes Between Moves"))
             ),
           mainPanel(
             plotOutput("distPlot"),
             br(),
             h2("Statistical Summary"),
             dataTableOutput("stats"),
             br(),
             h2("Data Table"),
             dataTableOutput("outputId")
           )
           )),
  tabPanel("Modeling", "contents"),
  tabPanel("Data", fluid = TRUE,
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("Columns","Columns",c(names(all_stats)),selected = c(names(all_stats))),
               downloadButton("downloadData", "Download entire Table as csv")
             ),
             mainPanel(
               h3("'Raw' Data Set"),
               dataTableOutput("Rawdata")
             ))
           )
           ))
)
