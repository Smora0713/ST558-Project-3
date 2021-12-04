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
library(tree)
library(caret)

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


all_stats_summary_WLD <- all_stats %>% dplyr::select(username,record.win,record.loss,record.draw,rank)

all_stats_summary_WLD <- gather(all_stats_summary_WLD,key = "record", value = "WLD",record.win,record.loss,record.draw) %>% arrange(username)

all_stats_summary_rating <- all_stats %>% dplyr::select(username,last.rating,best.rating,rank)

all_stats_summary_rating <- gather(all_stats_summary_rating,key = "record", value = "Rating",last.rating,best.rating) %>% arrange(username)


#-----------------------------------------------------------------------------------------------------


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

#Data Exploration
  updateSelectInput(session, "User", 
                    choices = sort(unique(all_stats_summary_WLD$username)), 
                    selected = sort(unique(all_stats_summary_WLD$username))[1]
  )
  
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
    },rownames = FALSE)
    output$stats <- renderDataTable({
      stats <- summary_tables %>% dplyr::select(values,input$stat) %>% filter(values == c(input$Values))
    },rownames = FALSE)
    datasetInput <- reactive(all_stats[,input$Columns])


    output$Rawdata <- renderDataTable({
      datasetInput() %>% 
        datatable(extensions = 'Buttons',
                  options = list(
                    dom = "lfrtipB",
                    buttons = c("copy", "csv", "pdf")),
                  filter = list(position = 'top'),
                  rownames = FALSE)
      },rownames = FALSE)
  output$models <- renderPlot({
    if(input$model_pick == "Multiple Linear Regression"){
      # Build the model
      # Split the data into training and test set
      set.seed(123)
      
      #Setting up the data split for cross validation
      train <- sample(1:nrow(all_stats), size = nrow(all_stats)*(input$percent_for_Train/100))
      test <- dplyr::setdiff(1:nrow(all_stats), train)
      
      all_stats_Train <- all_stats[train,]
      all_stats_Test <- all_stats[test,]
      
      fit_formula <- paste("rank", "~", paste(input$Variables_for_models, collapse = "+")) %>% as.formula()
      fit <- lm(fit_formula,all_stats_Train)

      tmp <- all_stats_Train %>% dplyr::select(rank,best.rating,last.rating,record.win,record.loss,record.draw,minutes_between_moves) %>%
        dplyr::mutate(fits=fitted(fit),
                      resids=resid(fit),
                      sresids=rstudent(fit))
      
       ggplot(data=tmp,mapping=aes(x=fits,y=resids)) +
        geom_point() +
        geom_hline(yintercept=0,linetype="dashed")
    }
    else if(input$model_pick == "Classification Tree"){
      # Split the data into training and test set
      set.seed(123)
      
      #Setting up the data split for cross validation
      train <- sample(1:nrow(all_stats), size = nrow(all_stats)*(input$percent_for_Train/100))
      test <- dplyr::setdiff(1:nrow(all_stats), train)
      
      all_stats_Train <- all_stats[train,]
      all_stats_Test <- all_stats[test,]
      
      fit_formula <- paste("rank", "~", paste(input$Variables_for_models, collapse = "+")) %>% as.formula()
      fitTree <- tree(fit_formula,all_stats_Train)
      
      plot(fitTree)
      text(fitTree)
    }
    else if(input$model_pick == "Random Forrest Model"){
      # Split the data into training and test set
      set.seed(123)
      
      #Setting up the data split for cross validation
      train <- sample(1:nrow(all_stats), size = nrow(all_stats)*(input$percent_for_Train/100))
      test <- dplyr::setdiff(1:nrow(all_stats), train)
      
      all_stats_Train <- all_stats[train,]
      all_stats_Test <- all_stats[test,]

      fit_formula <- paste("rank", "~", paste(input$Variables_for_models, collapse = "+")) %>% as.formula()

      rfFit <- train(fit_formula, data = all_stats_Train,
                     method = "rf",
                     trControl = trainControl(method = "cv",
                                              number = 5),
                     tuneGrid = data.frame(mtry = 1:input$tuneGrid))

      plot(rfFit)
    }
  })
  
  output$Model_Accuracy <- renderDataTable({
    if(input$model_pick == "Multiple Linear Regression"){
    # Split the data into training and test set
    set.seed(123)
    
    #Setting up the data split for cross validation
    train <- sample(1:nrow(all_stats), size = nrow(all_stats)*(input$percent_for_Train/100))
    test <- dplyr::setdiff(1:nrow(all_stats), train)
    
    all_stats_Train <- all_stats[train,]
    all_stats_Test <- all_stats[test,]
    
    fit_formula <- paste("rank", "~", paste(input$Variables_for_models, collapse = "+")) %>% as.formula()
    fit <- lm(fit_formula,all_stats_Train)
    
    predictions <- fit %>% predict(all_stats_Test)
    
    Model_Accuracy <- data.frame( R2 = R2(predictions, all_stats_Test$rank),
                RMSE = RMSE(predictions, all_stats_Test$rank),
                MAE = MAE(predictions, all_stats_Test$rank))
    }else if(input$model_pick == "Classification Tree"){
      # Split the data into training and test set
      set.seed(123)
      
      #Setting up the data split for cross validation
      train <- sample(1:nrow(all_stats), size = nrow(all_stats)*(input$percent_for_Train/100))
      test <- dplyr::setdiff(1:nrow(all_stats), train)
      
      all_stats_Train <- all_stats[train,]
      all_stats_Test <- all_stats[test,]

      fit_formula <- paste("rank", "~", paste(input$Variables_for_models, collapse = "+")) %>% as.formula()
      fitTree <- tree(fit_formula,all_stats_Train)

      predictions <- fitTree %>% predict(all_stats_Test)
      
      data.frame( R2 = R2(predictions, all_stats_Test$rank),
                  RMSE = RMSE(predictions, all_stats_Test$rank),
                  MAE = MAE(predictions, all_stats_Test$rank))
    } else if(input$model_pick == "Random Forrest Model"){
      # Split the data into training and test set
      set.seed(123)
      
      #Setting up the data split for cross validation
      train <- sample(1:nrow(all_stats), size = nrow(all_stats)*(input$percent_for_Train/100))
      test <- dplyr::setdiff(1:nrow(all_stats), train)
      
      all_stats_Train <- all_stats[train,]
      all_stats_Test <- all_stats[test,]
      
      fit_formula <- paste("rank", "~", paste(input$Variables_for_models, collapse = "+")) %>% as.formula()
      
      rfFit <- train(fit_formula, data = all_stats_Train,
                     method = "rf",
                     trControl = trainControl(method = "cv",
                                              number = 5),
                     tuneGrid = data.frame(mtry = 1:input$tuneGrid))

      predictions <- rfFit %>% predict(all_stats_Test)
      
      data.frame( R2 = R2(predictions, all_stats_Test$rank),
                  RMSE = RMSE(predictions, all_stats_Test$rank),
                  MAE = MAE(predictions, all_stats_Test$rank))
      }
  },rownames = FALSE)
  output$model_description <- renderUI({
    if(input$model_pick == "Multiple Linear Regression"){
      withMathJax(
        helpText("The following Multiple Linear Regression helps us understand the relationship between our independent variables (e.g. best rating, record wins, etc.) to our dependent variable (Rank). Ultimately we do this to be able to predict where a new player would rank given the variables that we have at hand. The following model can be read as such:
                 
                 $$Y = \\beta_0 + \\beta_1 Best.Rating + \\beta_2 Last.Rating + \\beta_3 Record.Win + \\beta_4 Record.Loss + \\beta_5 Record.Draw + \\beta_6 Minutes \\space Between \\space moves$$
                 
                 From the model above we are able to see which variable we should really focus on when making out predictions.")
      )
    }else if(input$model_pick == "Classification Tree"){
      withMathJax(
        helpText("This regression/classification tree model allows for us to split our data based on the key variables that the decisiion tree algorithm deems to be the best split. Ultimately this is one of the foundametanls that we can utilize for our a Random Forest Model by being able to manipulate the number of nodes in our tree. In our case we will focus on a regression tree since we are predicting a continous response. Meaning that we split our prediction into specific regions, we can usually use the mean of observations as predictions.Compared to a SLR we see that a tree will seperate our data prior to making these predictions. By splitting the data into regions we have more flexibility in our prediction but we should also expect more variance.")
      )
    }else if(input$model_pick == "Random Forrest Model"){
      withMathJax(
        helpText("Random Forest, boosting, and bagging are three methods that average across trees. Lose interpetability but gain in prediction! This is proving to be true for our Random Forest model specifically. A Random Forest can be compared to bagging but instead of using all of our predictors in our trees we will use a random subset of predictors. This way we use different individual tree based models aggregated over large numbers. The reason we would not want to always utilize all of our predictors:"),
                 tags$li("If a really strong predictor exists, tree will probably use it for the first split"),
                 tags$li("Makes bagged trees predictions more correlated"),
                 tags$li("By randomly selecting the predictors one or two predictors will not dominate our entire model")
      )
    }
  })
  # Predictions
  output$prediction <- renderText({
    new_data <- data.frame(best.rating = input$best.rating.Slide, 
                           last.rating = input$last.rating.Slide,
                           record.win = input$record.win.Slide, 
                           record.loss = input$record.loss.Slide, 
                           record.draw = input$record.draw.Slide, 
                           minutes_between_moves = input$minutes_between_moves.Slide)
    if(input$model_pick == "Multiple Linear Regression"){
      # Split the data into training and test set
      set.seed(123)
      
      #Setting up the data split for cross validation
      train <- sample(1:nrow(all_stats), size = nrow(all_stats)*(input$percent_for_Train/100))
      test <- dplyr::setdiff(1:nrow(all_stats), train)
      
      all_stats_Train <- all_stats[train,]
      all_stats_Test <- all_stats[test,]
      
      fit_formula <- paste("rank", "~", paste(input$Variables_for_models, collapse = "+")) %>% as.formula()
      fit <- lm(fit_formula,all_stats_Train)
      
      rank <- predict(fit, newdata = new_data)
    paste0("The predicted rank given the inputs is: ", if(rank <= 1){
      return(1)
      }else if(rank > 1 & rank <= 50){
        return(round(rank))}
      else if(rank > 50){
        return(50)})}
    else if(input$model_pick == "Classification Tree"){
      # Split the data into training and test set
      set.seed(123)
      
      #Setting up the data split for cross validation
      train <- sample(1:nrow(all_stats), size = nrow(all_stats)*(input$percent_for_Train/100))
      test <- dplyr::setdiff(1:nrow(all_stats), train)
      
      all_stats_Train <- all_stats[train,]
      all_stats_Test <- all_stats[test,]
      
      fit_formula <- paste("rank", "~", paste(input$Variables_for_models, collapse = "+")) %>% as.formula()
      fitTree <- tree(fit_formula,all_stats_Train)
      
      rank <- predict(fitTree, newdata = new_data)
      
      paste0("The predicted rank given the inputs is: ", if(rank <= 1){
        return(1)
      }else if(rank > 1 & rank <= 50){
        return(round(rank))}
      else if(rank > 50){
        return(50)})
    }    else if(input$model_pick == "Random Forrest Model"){
      # Split the data into training and test set
      set.seed(123)
      
      #Setting up the data split for cross validation
      train <- sample(1:nrow(all_stats), size = nrow(all_stats)*(input$percent_for_Train/100))
      test <- dplyr::setdiff(1:nrow(all_stats), train)
      
      all_stats_Train <- all_stats[train,]
      all_stats_Test <- all_stats[test,]
      
      fit_formula <- paste("rank", "~", paste(input$Variables_for_models, collapse = "+")) %>% as.formula()
      
      rfFit <- train(fit_formula, data = all_stats_Train,
                     method = "rf",
                     trControl = trainControl(method = "cv",
                                              number = 5),
                     tuneGrid = data.frame(mtry = 1:input$tuneGrid))
      
      rank <- predict(rfFit, newdata = new_data)
      
      paste0("The predicted rank given the inputs is: ", if(rank <= 1){
        return(1)
      }else if(rank > 1 & rank <= 50){
        return(round(rank))}
      else if(rank > 50){
        return(50)})
    }
    
  })
  
    output$downloadData <- downloadHandler(
      filename = function(){paste("Chess_Top_Data", ".csv", sep = "")},
      content = function(file){write.csv(datasetInput(), file)}
    )
})