library(httr)
library(tidyverse)
library(jsonlite)
library(data.table)
library(shiny)
library(DT)
library(shinyWidgets)

ui <-  fluidPage( setBackgroundColor(
    color = c("#F7FBFF", "#b3d5f2"),
    gradient = "linear",
    direction = "bottom"
  ),
                 tabsetPanel(
                   tabPanel(title = "What is this voodoo?",
                            h3("I was looking for a job and then I found a job..."),
                            br(),
                            p("This R Shiny app allows you to search USAjobs.gov for:"),
                            br(),
                            ("1. A list of keywords, with an implicit 'OR' operator between them"),
                            br(),
                            ("2. Filter out results using another list of keywords"),
                            br(),
                            ("3. Specify how many days back you want to look, in terms of the application opening date."),
                            p(),
                            ("To be clear, it only returns each result once, even if it is returned by multiple searches."),
                            p(),
                            ("Let me stress that this won't be very useful for random searching.  This is just an efficient way to view the same group of search results every day, so you can quickly catch any new postings that you might be interested in applying for, across all your searches of interest.  So get your ducks in a row by searching on the USAjobs.gov website, and then run this as frequently as you wish to see new results.")),
    tabPanel("Search Panel",
             fileInput("file", 
                "User Info", 
                buttonLabel = "Select File", accept = ".csv"),
      textInput("keywords", "Search for these words\n(separate with commas):", ""),
      textInput("skipwords", "Exclude jobs with this word in the title (separate with commas):", ""),
      numericInput("days", "Days since job was posted", 0, min = 0),
      actionButton("search", label = "Commencing Countdown, Engines On...Click to Search."),
     ),
    tabPanel(title = "Search Results",
             DT::dataTableOutput("mytable"))
  )
)


server <- function(input, output) {
  
  login <- reactive({inFile <- input$file
  if (is.null(inFile))
      return(NULL)
  read.csv(inFile$datapath, header = FALSE)
  #print(read.csv(inFile$datapath, header = FALSE))
  })
  
  observeEvent(input$search, {
    post_days <- input$days
    KW_list <- strsplit(input$keywords,",|, ") %>% unlist()
    skip_list <- strsplit(input$skipwords,",|, ") %>% unlist()
    URL_list <- paste0("https://data.usajobs.gov/api/search?Keyword=",KW_list,"&ResultsPerPage=1000")
    
    #Looped requests
    GETfunc <- function (URL){
      GET(url = URL, 
          config = add_headers("Host" = "data.usajobs.gov", 
                         "User-Agent" = login()$V1,
                         "Authorization-Key" = login()$V2))
      }
    
    #Conduct Searches
    r.list <- lapply(URL_list, GETfunc)
    
    #Make into data frames
    df.list <- list()
    
    for (i in 1:length(r.list)){
      
      df.list[[i]] <- content(r.list[[i]], "text") %>% 
        fromJSON()
      }
    
    #Clean and organize
    results.list <- list()
    
    for (i in 1:length(df.list)){
      results.list[[i]] <- df.list[[i]]$SearchResult$SearchResultItems$MatchedObjectDescriptor %>% 
        filter(!(str_detect(tolower(PositionTitle), paste0(skip_list, collapse = "|"))), 
               PositionStartDate > Sys.Date() - post_days) %>% #date minus 1.5 days 
        select(PositionID,
               OrganizationName,
               ApplyURI,
               PositionTitle,
               PositionLocation,
               QualificationSummary,
               UserArea)
      }
    
    #create final table (originally for xtable html)
    results.html.list <- list()
    
    #Subset to non-null results
    results.list <- results.list[lapply(results.list, nrow) %>%
                                   unlist() > 0]
    
    for (i in 1:length(results.list)){
      results.html.list[[i]] <- data.frame("Org" = results.list[[i]]$OrganizationName,
                                           "Title" = results.list[[i]]$PositionTitle,
                                           "Locations" = lapply(results.list[[i]]$PositionLocation, 
                                                                function(x){
                                                                  x %>% 
                                                                    select(LocationName) %>%
                                                                    unlist() %>%
                                                                    paste(., collapse = " <br><br>•")
                                                                  }) %>%
                                             unlist(),
                                           "Summary" = lapply(results.list[[i]]$UserArea$Details$MajorDuties,
                                                              function(x){
                                                                x %>%
                                                                  paste(., collapse = "<br><br>•")
                                                                }) %>%
                                             unlist(),
                                           "URL" = unlist(results.list[[i]]$ApplyURI) %>% 
                                             paste("<a href=\"",.,"\">Link</a>")
      )
      }
    
    #Bind results and find unique
    done_zo <- rbindlist(results.html.list) %>% 
      unique.data.frame() %>% 
      arrange(Org)
    
    output$mytable = DT::renderDataTable({
    done_zo
  }, 
  escape = FALSE,
  )
  })
}
  
shinyApp(ui, server)
