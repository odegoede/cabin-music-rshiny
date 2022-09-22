##
## R Shiny App: Team Honest Bangers
##

## CURRENT STEP: ADD MORE PLOTS (MOST POPULAR ARTISTS, DUPLICATE SELECTIONS) 
## NEXT STEPS: FRIEND PROFILES, TENDER RECOLLECTIONS SUMMARIES

# Load libraries
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(ggbeeswarm)
library(ggstance)
library(stringi)

# Load source code
source(file = "R/plot_functions.R")

# While there's an unnecessary warning around setting boxgap with plotly, suppress warnings
# (keep commented out until app is complete, to look for other warnings)
# options(warn = -1)

# Load music data, set up colour palette
dataset <- readxl::read_xlsx("data/cabin_music_2022.xlsx")

friend_val <- dataset %>% pull(friend) %>% unique() %>% sort()
friend_val <- c(friend_val[-which(friend_val == "Team Honest")], "Team Honest")
friend_pal <- read.table("data/colour_map.txt", sep = "\t", header = T)
friend_pal <- friend_pal$colour %>% set_names(friend_pal$friend)

dataset <- dataset %>% mutate(friend = factor(friend, levels = friend_val))
dataset <- dataset %>% mutate(song_length = (song_length_lostColon%/%100*60 + song_length_lostColon%%100))

# Defining dashboard header
header <- dashboardHeader(title = "Team Honest")

# Defining dashboard sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Music Profiles", tabName = "music", icon = icon("music")),
    menuItem("Explore the Data", tabName = "explore", icon = icon("table")),
    menuItem("Friend Profiles", tabName = "friends", icon = icon("users"),
             badgeLabel = "coming soon!", badgeColor = "aqua"),
    menuItem("Tender Recollections (2019)", tabName = "tender", icon = icon("heart"),
             badgeLabel = "coming soon!", badgeColor = "aqua")
  )
)

# Defining dashboard body
body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  tabItems(
    # Main dashboard
    tabItem(tabName = "music",
            
            h1("Team Honest Music, 2012-2022"),
            
            h2("How many songs has each friend contributed?"),
            
            pickerInput("num_song_friend", "Pick some friends:", choices = friend_val,
                        multiple = TRUE,
                        selected = friend_val,
                        options = list(`actions-box` = TRUE)),
            
            plotlyOutput("num_song_plot", height = "600px", width = "800px"),
            
            h2("How long are our songs?"),
            
            pickerInput("song_length_year", "Which year(s)?:", choices = unique(dataset$year_chr),
                        multiple = TRUE,
                        selected = unique(dataset$year_chr),
                        options = list(`actions-box` = TRUE)),
            
            plotlyOutput("song_length_plot", height = "800px", width = "800px")
    ),
    
    # Peek at the data
    tabItem(tabName = "explore",
            
            h1("Explore the data"),
            
            DT::dataTableOutput("data_peek")
    )
  )
)


# Bringing dashboard pieces together in UI
ui <- dashboardPage(title = "Honest Bangers Analysis",
                    header = header, sidebar = sidebar, body = body)


# Defining behaviour
server <- function(input, output) {
  
  output$data_peek <- DT::renderDataTable(select(dataset, c("friend", "song_title", "artist", "playlist", "year_num", "song_length", "tender_recollections_2019")), 
                                      filter = list(position = "top", plain = TRUE),
                                      options = list(pageLength = 10))

  output$num_song_plot <- renderPlotly(get_num_song_plot(df = get_plot_dat(dataset, "friend", input$num_song_friend), pal = friend_pal))

  output$song_length_plot <- renderPlotly(get_song_length_plot(df = get_plot_dat(dataset, "year_chr", input$song_length_year), pal = friend_pal))

  }

# Execute
shinyApp(ui, server)
