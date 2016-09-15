library(shiny)
library(RCurl)
library(ggplot2)
library(lubridate)
library(data.table)

filepath <- 'https://raw.githubusercontent.com/bibzzzz/tw_case_study/master/referral_weekly_data.csv'

db_url <- getURL(filepath)
db_init_table <- read.csv(text = db_url)
db_init_table[db_init_table=='\\0'] <- NA
db_init_table$invite_sent_timestamp <- dmy_hm(db_init_table$invite_sent_timestamp) #convert to date format

# Define server logic required to generate dashboard
shinyServer(function(input, output, session) {
  
  #read / format init table:
  db_output_table <- reactive({
    table <- data.frame(db_init_table)
    # max_data <- max(table$invite_sent_timestamp)
    table$weeks_since <- floor(as.numeric(difftime(input$date,table$invite_sent_timestamp,units="weeks")))
    table$invite_success <- ifelse(!is.na(table$id_user_referred), "Successful invite", NA)
    return (table)
  })
  
  #filter table:
  interim_output_table <- reactive({
    db_table <- db_output_table()
    rel_weeks <- unique(db_table$invite_sent_week[db_table$weeks_since>=0&db_table$weeks_since<=input$n_weeks])
    rel_weeks <- as.character(rel_weeks[order(rel_weeks)])
#     if (length(rel_weeks) > input$n_weeks){
#       rel_weeks <- rel_weeks[length(rel_weeks)-input$n_weeks:length(rel_weeks)]
#     }
    rel_week_ids <- as.numeric(sapply(strsplit(substring(rel_weeks, 1), "-"), "[[", 2))
    db_table$rel_week <- as.numeric(sapply(strsplit(substring(db_table$invite_sent_week, 1), "-"), "[[", 2))
    db_table$rel_year <- factor(sapply(strsplit(substring(db_table$invite_sent_week, 1), "-"), "[[", 1))
    db_table <- subset(db_table,user_addr_country==input$user_country&rel_week%in%rel_week_ids&user_age_years_bucket==input$user_age_bracket)    
#     # db_table <- subset(db_table,user_addr_country==input$user_country&user_age_years_bucket==input$user_age_bracket)    
    return (db_table)
  })

  
  #final output chart:
  output$valueplot <- renderPlot({ 
    input_data <- data.table(interim_output_table())
    input_data <- input_data[order(weeks_since)]
    input_data$split_var <- input_data[,c(paste0(input$breakdown_var)), with=FALSE]
    leg_cols <- ceiling(length(unique(input_data$split_var))/10)
    ggplot(input_data,aes(x=rel_week,fill=factor(split_var))) +
      geom_bar() +
      # scale_x_reverse(lim=c(input$n_weeks,0)) +
      xlab("\nWeek #") +
      ylab("Referral count\n") +
      facet_wrap(~rel_year,scales="fixed",ncol=2) +
      guides(fill=guide_legend(title=input$breakdown_var, ncol=leg_cols)) +
      theme(axis.text.x = element_text()
            , axis.ticks=element_blank())
  }, height = 600, width = 800)
})
