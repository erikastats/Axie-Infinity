library(shiny)
library(tidyverse)
# source("autenticacao.R")
library(googlesheets4)
library(DT)
library(shinyWidgets)

# library(googlesheets)

#Code to download a new API token commented here:
gs4_auth(
  cache = ".secrets",
  email = "erikanima3dk@gmail.com"
)
# saveRDS(token, "googlesheets_token.rds")


# Dados GoogleSheets ------------------------------------------------------

Linksheet <- "https://docs.google.com/spreadsheets/d/1SapeQjxtapWxXTgpzcoY-6CWd8KuDOtQzTiC6oLUZes/edit#gid=0"
sheet <- "Principal"



    UI = fluidPage(
      headerPanel('Axie')
      ,mainPanel(
        tabsetPanel(
          tabPanel("Ganho",
                   plotly::plotlyOutput("plotDiary")
                   ,DTOutput("diary_table"),
                   fluidRow(
                     column(width = 4, dateInput('date', "Choose today's date:", value = NULL, min = NULL, max = NULL,
                                                 format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                 language = "en" )),
                     column(4,numericInput("slp_rodada", label = h3("Submit SLP"), value = NA) ),
                     column(4, numericInput("rank_rodada", label = h3("Submit Rank"), value = NA))
                     ,column(8, textAreaInput("comment", "Comments")),
                     column(4
                            ,actionButton("action", label = "Submit Scores"))
                   ) ),
          tabPanel("Auxilio",
                   fluidRow(
                     column(4,
                            awesomeCheckboxGroup(width = "25%",
                                                          inputId = "mouth",
                                                          label = "Boca", 
                                                          choices = c("1", "2"),
                                                          selected = "",
                                                          inline = TRUE, 
                                                          status = "danger"
                   )),
                            column(4,
                                   awesomeCheckboxGroup(width = "25%",
                                                        inputId = "horn",
                                                        label = "Horn", 
                                                        choices = c("1", "2"),
                                                        selected = "",
                                                        inline = TRUE, 
                                                        status = "danger"
                                   )),
                            column(4,
                                   awesomeCheckboxGroup(width = "25%",
                                                        inputId = "back",
                                                        label = "Back", 
                                                        choices = c("1", "2"),
                                                        selected = "",
                                                        inline = TRUE, 
                                                        status = "danger"
                                   )),
                            column(4,awesomeCheckboxGroup(width = "25%",
                                                          inputId = "tail",
                                                          label = "Tail", 
                                                          choices = c("1", "2"),
                                                          selected = "",
                                                          inline = TRUE, 
                                                          status = "danger"
                            )))  ,
                   
                   actionButton(inputId = "updatechoices", label = "New round")
                   ),
          tabPanel("Table", tableOutput("table"))
        )
        
        # ,actionButton("save", label = "Save data")
        
        # ,htmlOutput("googleSheet"))
      ))
    
    
    SERVER = function(input, output, session) {     
     
      
      tableValues <- reactiveValues(df = data.frame(Date = as.Date(as.character()),
                                                    Number_game = as.numeric(),
                                                    SLP = as.numeric(),
                                                    Rank = as.numeric(),
                                                    Comentario = as.character(),
                                                    check.names = FALSE))
      
      observeEvent(input$action, {
        
        temp <- tableValues$m
        
        newRow <- data.frame(Date = input$date,
                             Number_game = input$action,
                             SLP = input$slp_rodada,
                             Rank = input$rank_rodada, 
                             Comentario = input$comment)
        
        
        if(!is.null(temp)) {
          
          if(isolate(input$date) < temp[nrow(temp), 1]) {
            temp <- rbind(temp, newRow)
            temp <- dplyr::arrange(temp, Date)
          } else {
            temp <- rbind(temp, newRow)
          }
        } else {
          temp <- rbind(temp, newRow)
        }
        
        tableValues$m <- temp
        
        write_csv(tableValues$m, "data.csv")
        dadosapp <-  read_csv("data.csv")
        
        # sheet_append(ss = Linksheet, data = dadosapp ,sheet =  sheet )
      })
      
      
      output$diary_table <- DT::renderDataTable({ tableValues$m })
      
      output$plotDiary <- plotly::renderPlotly({
        table_data <- tableValues$m
        if (input$action == 0){
          plotly::plot_ly(x = 0, y = 0, type = 'scatter', mode = 'lines+markers') %>% 
            plotly::config(displayModeBar = F)
        } else {
          table_data %>% 
            plotly::plot_ly(x = ~Number_game, y = ~Rank, type = 'scatter', mode = 'lines+markers') %>% 
            plotly::config(displayModeBar = F)}
      })
      
      observeEvent(input$updatechoices, {

        
        updateAwesomeCheckboxGroup(session = session, 
                             inputId = "mouth",
                             label = "Boca", 
                             choices = c("1", "2"),
                             selected = "",
                             inline = TRUE, 
                             status = "danger"
        )
        updateAwesomeCheckboxGroup(session = session, 
          inputId = "horn",
          label = "Horn", 
          choices = c("1", "2"),
          selected = "",
          inline = TRUE, 
          status = "danger"
        )
        updateAwesomeCheckboxGroup(session = session, 
          inputId = "back",
          label = "Back", 
          choices = c("1", "2"),
          selected = "",
          inline = TRUE, 
          status = "danger"
        )
        updateAwesomeCheckboxGroup(session = session, 
          inputId = "tail",
          label = "Tail", 
          choices = c("1", "2"),
          selected = "",
          inline = TRUE, 
          status = "danger"
        )
      }, ignoreInit = TRUE)
      
      # observeEvent(input$save,{
      #   
      #   # Linksheet %>% sheet_append(data = newRow ,sheet =  sheet )
      #   # Linksheet %>% sheet_append(data = isolate(tableValues$m) ,sheet =  sheet )
      #   write_csv(tableValues$m, "data.csv")
      #   dadosapp <-  read_csv("data.csv")
      #   
      #   sheet_append(ss = Linksheet, data = dadosapp ,sheet =  sheet )
      # 
      #     # reinitiating the page
      #      # tableValues$m <- reactiveValues(df = data.frame(Date = as.Date(as.character()),
      #      #                                               Number_game = as.numeric(),
      #      #                                               SLP = as.numeric(),
      #      #                                               Rank = as.numeric(),
      #      #                                               check.names = FALSE))
      # 
      # })
      
      
    }
  

shinyApp(
  ui = UI,
  server = SERVER
)

# dadosapp <- read_rds("data.r")
# Linksheet %>% sheet_append(data = dadosapp ,sheet =  sheet )
