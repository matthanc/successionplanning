library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(eeptools)
library(DT)
library(shinythemes)

factorlookup <- read_csv("factorlookup.csv")
load("retirementmodel.rda")

#user interface code
ui <- fluidPage(
  theme = shinytheme("spacelab"),
  br(),
  br(),
  titlePanel(
    h1("Succession Planning - Retirement Application", align = "center")
    ),
  br(),
  br(),
  fluidRow(column(1),
           column(10,
                  sidebarLayout(
                    sidebarPanel(fileInput("datafile",NULL, accept = c(".xlsx")),
                                 br(),
                                 downloadButton("downloadfile", "Download modified file"),
                                 br(),
                                 br(),
                                 h2("Instructions:", align = "center"),
                                 tags$ul(
                                   tags$li("In PeopleSoft, run query MRG_CITYWIDE_WORKFORCE and download in Excel (.xlsx) format"),
                                   tags$li("Load file into app (file loaded correctly if employee data populated to the right)"),
                                   tags$li("Click on 'download modified file' to download the file with retirement information and predictors")),
                    ),
                    mainPanel(
                      DT::dataTableOutput("filecontents"),
                      br(),
                    )
                  )),
           column(1),
  )
)

#server code
server <- function(input, output, session){

  filedata <- reactive({
    req(input$datafile)
    
    ext <- tools::file_ext(input$datafile$name)
    switch(ext,
           xlsx = readxl::read_excel(toString(input$datafile$datapath), skip = 1),
           validate("Invalid file, please upload an .xlsx file")
           )
  })
  
  output$filecontents <- renderDT({
    df <- filedata() %>%
      select(2, 4, 6)
    datatable(df)
  })
  
  output$downloadfile <- downloadHandler(
    filename = "successionplanningreport.csv",
    content = function(file) {
      df <- filedata()
      df <-  df %>% modify_if(is.POSIXct, as.Date)
      
      #calculate age and service years to nearest 0.25
      df <- df %>%
        mutate(age = (round((age_calc(df$Birthdate, enddate = today(), units = "years"))*4))/4) %>%
        mutate(service_years = (round((age_calc(df$`Start Date`, enddate = today(), units = "years"))*4))/4)
      
      #apply retirement factor to df and create RetirementFactor variable
      df <- df %>%
        left_join(factorlookup, by = c(age = 'key')) %>%
        rename(retirementfactor = value) %>%
        mutate(retirement_formula = pmin((service_years * retirementfactor),0.75))
      df <- df %>% mutate(retirementmodel = predict(retirementmodel, newdata = df, type = "response"))
      
      
      df <- df %>%
        mutate("Likelihood to Retire" = ifelse(retirementmodel < 0.001, "Very Low", ifelse(retirementmodel < 0.01, "Low", ifelse(retirementmodel < 0.30, "Moderate", ifelse(retirementmodel < 0.70, "High", ifelse(retirementmodel >= 0.70, "Very High", NA))))))
      
      #write csv file to folder
      df <- df %>%
        rename(Age = age) %>%
        rename("Service Years" = service_years) %>%
        rename("Retirement Factor (multiplier)" = retirementfactor) %>%
        rename("Retirement Formula" = retirement_formula) %>%
        rename("Retirement Index" = retirementmodel)
      write.csv(df, file)
    }
  )
  
}


#run shiny server
shinyApp(ui = ui, server = server)
