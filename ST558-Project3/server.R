#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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

#Reading in the data that we will need
#Utilizing the Chess.com API to pull in the top fifty players for each type of chess rules. We will begin with "Daily" (easiet to understand and most data on it) and we will usitlize that as the parameter. This first API will be utilized to pull more API's. It'll be a master table of sorts.
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


all_stats <- left_join(all_stats,country_codes,by = c("country" = "country_code")) %>% mutate(minutes_between_moves = round((record.time_per_move/60),2))

set.seed(123)
world <- map_data("world")

all_stats <-all_stats %>% mutate(Latitude..average.jit = jitter(all_stats$Latitude..average., amount = 3),Longitude..average.jit = jitter(all_stats$Longitude..average., amount = 3))



summary_tables <- rbind(
  all_stats %>% mutate(Means = mean(best.rating),Medians = median(best.rating),SDs = sd(best.rating)) %>% select(starts_with(c("Means","Medians","SDs"))) %>% distinct(),
  
  all_stats %>% mutate(Means = mean(last.rating),Medians = median(last.rating),SDs = sd(last.rating)) %>% select(starts_with(c("Means","Medians","SDs"))) %>% distinct(),
  
  all_stats %>% mutate(Means = mean(minutes_between_moves),Medians = median(minutes_between_moves),SDs = sd(minutes_between_moves)) %>% select(starts_with(c("Means","Medians","SDs"))) %>% distinct()
)

rownames(summary_tables) <- c("Best Rating","Last Rating","Minutes Between Moves")
summary_tables <- cbind(values = rownames(summary_tables), summary_tables)
rownames(summary_tables) <- 1:nrow(summary_tables)

#-----------------------------------------------------------------------------------------------------


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

#Data Exploration

    output$distPlot <- renderPlot({
      if(input$EDAType == "map"){
        # Create map
        ggplot() + geom_map(data = world, map = world,aes(long, lat, map_id = region),color = "white", fill = "lightgray", size = 0.1) + geom_point(data = all_stats,aes(Longitude..average.jit, Latitude..average.jit),alpha = 0.7) + geom_point(position=position_jitter(width=.1, height=.1))
      }else if(input$EDAType == "bar"){
      if(input$WLD_Rating == "Win, Loss, Draw"){
        #WLD data along with rank
        
        all_stats_summary_WLD_user <- all_stats_summary_WLD %>% filter(username == input$User)
        
        ggplot(all_stats_summary_WLD_user,aes(x = record,y=WLD)) + geom_bar(stat="identity") + ggtitle(paste0("Bar plot for user ",all_stats_summary_WLD_user$username," whose rank is: ",all_stats_summary_WLD_user$rank)) + xlab("") + ylab("")
        
        
      }else if(input$WLD_Rating == "Rating"){
        all_stats_summary_rating_user <- all_stats_summary_rating %>% filter(username == input$User)
        
        ggplot(all_stats_summary_rating_user,aes(x = record,y=Rating)) + geom_bar(stat="identity") + ggtitle(paste0("Bar plot for user ",all_stats_summary_rating_user$username," whose rank is: ",all_stats_summary_rating_user$rank)) + xlab("") + ylab("")
        
      }
      }else if(input$EDAType == "hist"){
        # Histogram of the last rating
        if(input$Rating == "Last"){
          ggplot(all_stats, aes(x=last.rating)) + 
          geom_histogram(alpha = 0.75) + geom_vline(xintercept = 2500, linetype = "dashed", color = "red") + 
          geom_text(aes(x=2500, label="\nGM rating", y=7), colour="Red", text=element_text(size=11)) + 
          geom_vline(xintercept = 2400, linetype = "dashed", color = "red") + 
          geom_text(aes(x=2400, label="\nIM rating", y=7), colour="blue", text=element_text(size=11)) +
          ylab("") +
          xlab("")
        }else if(input$Rating == "Best"){
          # Histogram of the best rating
          ggplot(all_stats, aes(x=best.rating)) + 
            geom_histogram(alpha = 0.75) + geom_vline(xintercept = 2500, linetype = "dashed", color = "red") + 
            geom_text(aes(x=2500, label="\nGM rating", y=7), colour="Red", text=element_text(size=11)) + 
            geom_vline(xintercept = 2400, linetype = "dashed", color = "red") + 
            geom_text(aes(x=2400, label="\nIM rating", y=7), colour="blue", text=element_text(size=11)) +
            ylab("") +
            xlab("")
        }else if(input$Rating == "Time"){
          # Average time spent between moves
          ggplot(all_stats, aes(x=minutes_between_moves)) + 
            geom_histogram(alpha = 0.75) +
            ylab("") +
            xlab("Minutes between moves")
        }
        }
      })
    output$outputId <- renderDataTable({
      if(input$EDAType == "map"){
        table1 <- data.frame(table(all_stats$Country))
        colnames(table1) <- c("Country/Region","# of ranked players")
        outputId <- table1
      }else if(input$EDAType == "bar"){
        if(input$WLD_Rating == "Win, Loss, Draw"){
        outputId <- all_stats %>% dplyr::select(username,record.win,record.loss,record.draw,rank)
        }else if(input$WLD_Rating == "Rating"){
          outputId <- all_stats %>% dplyr::select(username,last.rating,best.rating,rank)
        }
      }else if(input$EDAType == "hist"){
        if(input$Rating == "Last"){
          all_stats %>% select(username,last.rating,rank)
        }else if(input$Rating == "Best"){
          all_stats %>% select(username,best.rating,rank)
        }else if(input$Rating == "Time"){
          all_stats %>% select(username,minutes_between_moves,rank)
        }
      }
    })
    output$stats <- renderDataTable({
      stats <- summary_tables %>% dplyr::select(values,input$stat) %>% filter(values == c(input$Values))
    })
    datasetInput <- reactive(all_stats[,input$Columns])


    output$Rawdata <- renderDataTable({
      datasetInput() %>% 
        datatable(extensions = 'Buttons',
                  options = list(
                    dom = "lfrtipB",
                    buttons = c("copy", "csv", "pdf")),
                  filter = list(position = 'top'),
                  rownames = FALSE)
      })


    output$downloadData <- downloadHandler(
      filename = function(){paste("Chess_Top_Data", ".csv", sep = "")},
      content = function(file){write.csv(datasetInput(), file)}
    )
})