library(httr)
library(tidyverse)
library(jsonlite)
library(xtable)
library(data.table)
library(shiny)
library(DT)

#load login info
login <- read.csv("login.csv", header = FALSE)

#Keyword List
KW_list <- c("Biology","Biologist", "Microbial", "Stennis", "Data%20Science", "Data%20Scientist","Ecology")
URL_list <- paste0("https://data.usajobs.gov/api/search?Keyword=",KW_list,"&ResultsPerPage=1000")

#Looped requests
GETfunc <- function (URL){
  GET(url = URL, 
    config = add_headers("Host" = "data.usajobs.gov", 
                         "User-Agent" = login$V1,
                         "Authorization-Key" = login$V2))
  }

r.list <- lapply(URL_list, GETfunc)

df.list <- list()

for (i in 1:length(r.list)){
  
  df.list[[i]] <- content(r.list[[i]], "text") %>% 
  fromJSON()
}

results.list <- list()

for (i in 1:length(df.list)){

results.list[[i]] <- df.list[[i]]$SearchResult$SearchResultItems$MatchedObjectDescriptor %>% 
  filter(!(str_detect(tolower(PositionTitle), "medical|clinical|pathology|hygien|dental|health")), 
         PositionStartDate > Sys.Date() - 2) %>% #date minus 1.5 days 
  select(PositionID,
         OrganizationName,
         ApplyURI,
         PositionTitle,
         PositionLocation,
         QualificationSummary,
         UserArea)
}

results.html.list <- list()

#Subset to non-null results
results.list <- results.list[lapply(results.list, nrow) %>% 
  unlist() > 0]

for (i in 1:length(results.list)){

results.html.list[[i]] <- data.frame("Org" = results.list[[i]]$OrganizationName,
                           "Title" = results.list[[i]]$PositionTitle,
                           "Locations" = lapply(results.list[[i]]$PositionLocation, function(x){
                             x %>% 
                               select(LocationName) %>%
                               unlist() %>% 
                               paste(., collapse = " <br><br>•") 
                           }) %>% 
                             unlist(),
                           "Summary" = lapply(results.list[[i]]$UserArea$Details$MajorDuties, function(x){
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

#Shiny App

ui <- basicPage(
  h2("Go get that Job! Today is the Day!"),
  DT::dataTableOutput("mytable")
)

server <- function(input, output) {
  output$mytable = DT::renderDataTable({
    done_zo
  }, 
  escape = FALSE,
  )
}
  
shinyApp(ui, server)
