library(shiny)
library(lubridate)
library(RCurl)

db_url <- getURL(filepath)
db_init_table <- read.csv(text = db_url)
db_init_table[db_init_table=='\\0'] <- NA
db_init_table$invite_sent_timestamp <- dmy_hm(db_init_table$invite_sent_timestamp) #convert to date format

max_date <- max(db_init_table$invite_sent_timestamp)
max_date <- paste(year(max_date),month(max_date),day(max_date),sep="-")

user_country_levels <- as.character(unique(db_init_table$user_addr_country))
user_age_levels <- as.character(unique(db_init_table$user_age_years_bucket))

breakdown_vars <- c('user_referred_addr_country', 'user_referred_age_years_bucket', 'invite_success')

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("TransferWise Weekly Referral Dashboard"),
  br(),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      h3("Dashboard settings"),
      dateInput("date", "Date:", value = max_date, format = "mm/dd/yy"),
      sliderInput("n_weeks","Select no. weeks to look back:", min=1, max=12, value = 12, step=1),
      selectInput("user_country", "user_country:",
                  user_country_levels),
      selectInput("user_age_bracket", "user_age_bracket:",
                  user_age_levels),
      selectInput("breakdown_var", "breakdown_var:",
                  breakdown_vars)
    ),
    
    mainPanel(
      plotOutput("valueplot"),
      div(tableOutput("player_proj_table"))
    )
  )
))