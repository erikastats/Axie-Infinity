
# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(argonDash)
library(shiny)
library(argonR)
library(lubridate)
library(DT)
library(shinyWidgets)


# Autenticacao ------------------------------------------------------------

source("autenticacao.R")


# Dados GoogleSheets ------------------------------------------------------

Linksheet <- "https://docs.google.com/spreadsheets/d/1SapeQjxtapWxXTgpzcoY-6CWd8KuDOtQzTiC6oLUZes/edit#gid=0"
sheet <- "Principal"
# dadosapp <- data.frame(Date = today(), Number_game = 1, SLP = 9, Rank = 1324)
# Linksheet %>% sheet_append(data = dadosapp ,sheet =  sheet )
# AxieRaw <- read_sheet(Linksheet, sheet )


# Última data -------------------------------------------------------------


# Data.frame vazio --------------------------------------------------------

# df_Dia <- data.frame(Date = date(character()),
#                      Partida =numeric(),
#                      SLP = numeric(),
#                      Rank = numeric() )
# Criando o dashboard -----------------------------------------------------



# Sidebar -----------------------------------------------------------------

Sidebar <- argonDashSidebar(
  vertical = TRUE,
  skin = "light",
  background = "white",
  size = "md",
  side = "left",
  id = "sidebar_axie",
  # brand_url = "http://www.google.com",
  brand_logo = "https://miro.medium.com/max/1100/0*MSmp3gLEbtHbMloT.png",

  argonSidebarHeader(title = "Main Menu"),
  argonSidebarMenu(
    argonSidebarItem(
      tabName = "info",
      icon = argonIcon(name = "tv-2", color = "info"),
      "Info"
    ),
    argonSidebarItem(
      tabName = "diario",
      icon = argonIcon(name = "tv-1", color = "green"),
      "Diario"
    )
  ),
  argonSidebarDivider(),
  argonSidebarHeader(title = format(today(), "%a %d/%m/%Y") %>% str_to_title())
)

# Header ------------------------------------------------------------------

Header <- argonHeader <- argonDashHeader(
  gradient = FALSE,
  color = "secondary",
  top_padding = 8,
  bottom_padding = 8,
  mask = TRUE,
  background_img = "https://exame.com/wp-content/uploads/2021/08/Axie-Infinity-game.jpg",
  opacity = 6,
  argonH1("Axie Dashboard Controler", display = 1) %>% argonTextColor(color = "white"),
  # argonLead("This is the content.") %>% argonTextColor(color = "white")
)

# Body --------------------------------------------------------------------

Body <- argonDashBody(
  argonTabItems(
    #info
    argonTabItem( tabName = "info"
                  # ,
                  )
    #diario
    ,argonTabItem( tabName = "diario",titlePanel("Axie Infinity diary registry")
                  # ,
                  ,argonRow(
                    argonColumn(width = 6
                      ,textOutput("rodada")
                           ,dateInput('date', "Choose today's date:", value = NULL, min = NULL, max = NULL,
                                      format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                      language = "en", width = NULL)
                           ,numericInput("slp_rodada", label = h3("Submit SLP score"), value = NA),
                           numericInput("rank_rodada", label = h3("Submit Rank score"), value = NA),
                           actionButton("action", label = "Submit Scores") ),
                    argonColumn(width = 6,
                           plotly::plotlyOutput("rank_plot"),
                           argonRow(
                             argonUser(
                               title = textOutput("totalslp"),
                               subtitle = "TOTAL SLP",
                               src = "slp.jpg"
                             ),
                             argonUser(
                               title = textOutput("lastrank"),
                               subtitle = "Last Rank",
                               src = "axl.jpg"
                             )
                           )
                              )),
                  # hr(),
                  DTOutput("diary_table"),
                  useSweetAlert(theme = "bulma"),
                  actionBttn(
                    inputId = "savedf",
                    label = "Save", 
                    style = "material-flat",
                    color = "royal"
                  )
                  )

  )
)

# Footer ------------------------------------------------------------------

Footer <- argonDashFooter(
  copyrights = "Érika S.M.B., 2021",
  # src = "https://github.com/DivadNojnarg",
  # argonFooterMenu(
  #   argonFooterItem("RinteRface", src = "https://github.com/RinteRface"),
  #   argonFooterItem("argon", src = "https://demos.creative-tim.com/argon-design-system/index.html")
  # )
)

# UI ----------------------------------------------------------------------

UI <- argonDashPage(
  title = "Axie App",
  author = "Érika S.M.B.",
  description = "Axie dashboard control",
  sidebar = Sidebar,
  # navbar = argonNav, 
  header = Header,
  body = Body,
  footer = Footer
)


# Server ------------------------------------------------------------------

SERVER <- function(input, output, session) {
  
  output$rodada <- renderText({ paste0("#", input$action +1 ) })
  
  tableValues <- reactiveValues(df = data.frame(Date = as.Date(as.character()),
                                                `Number game` = as.numeric(),
                                                SLP = as.numeric(),
                                                Rank = as.numeric(),
                                                check.names = FALSE))
  
  observeEvent(input$action, {
    
    temp <- tableValues$m
    
    newRow <- data.frame(Date = input$date,
                         Number_game = input$action,
                         SLP = input$slp_rodada,
                         Rank = input$rank_rodada)
    
    Linksheet %>% sheet_append(data = isolate(newRow),sheet =  sheet )
    
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
    
  })
  
  # table_data <- tableValues$m
  
  
  output$diary_table <- DT::renderDataTable({

      
      table_data <- tableValues$m

    table_data
    
  })
  
  output$rank_plot <- plotly::renderPlotly({
    
    table_data <- tableValues$m
    if (input$action == 0){
      plotly::plot_ly(x = 0, y = 0, type = 'scatter', mode = 'lines+markers') %>% 
        plotly::config(displayModeBar = F)
    } else {
    table_data %>% 
      plotly::plot_ly(x = ~`Number game`, y = ~Rank, type = 'scatter', mode = 'lines+markers') %>% 
      plotly::config(displayModeBar = F)}
  })
  
  
  output$totalslp <-  renderText({ 
    table_data <- tableValues$m
    if (input$action == 0){
      text = "0"
    } else {
      text = table_data %>% select(SLP) %>% pull() %>%  sum() %>% as.character()}
    text
    })
  
  output$lastrank <-  renderText({ 
    
    table_data <- tableValues$m
    if (input$action == 0){
      text = "0"
    } else {
      text =  table_data %>% select(Rank) %>% pull() %>% tail(1) %>% as.character()}
    text
    })
  # table_data <- eventReactive(input$savedf, {
  #   
  #   tableValues$m %>% as.data.frame()})
  
  # observeEvent(input$savedf, {
  #   
  #   # table_data <- isolate(tableValues$m) %>% as.data.frame()
  #   
  #   # append new rows to the google sheet
  #   Linksheet %>% sheet_append(tableValues$m %>% as.data.frame(), sheet )
  #   
  #   # reinitiating the page
  #   # tableValues <- reactiveValues(df = data.frame(Date = as.Date(as.character()),
  #   #                                               `Number game` = as.numeric(),
  #   #                                               SLP = as.numeric(),
  #   #                                               Rank = as.numeric(),
  #   #                                               check.names = FALSE))
  #   
  #   # updateNumericInput(session, inputId = "slp_rodada",  label = h3("Submit SLP score"), value = NA)
  #   # updateNumericInput(session,"rank_rodada", label = h3("Submit Rank score"), value = NA)
  #   # updateActionButton(session, "action", label = "Submit Scores")
  # 
  #   
  #   # sendSweetAlert(
  #   #   session = session,
  #   #   title = "Success !!",
  #   #   text = paste0("Progress day ", today(), " is safe!"),
  #   #   type = "success"
  #   # )
  #   
  #   
  # })
  
}


# App ---------------------------------------------------------------------

shinyApp(
  ui = UI,
  server = SERVER
)

