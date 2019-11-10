library(shiny)
library(leaflet)
library(RDSTK)
library(shinythemes)
library(shinyjs)
library(readxl)
library(zipcode)
library(reticulate)
library(stringr)
library(plyr)
library(tibble)
library(DBI)

# Helper for getting new connection to Cloud SQL
getSqlConnection <- function(){
  con <-
    dbConnect(
      RMySQL::MySQL(),
      username = 'root',
      password = 'helloangel',
      host = '35.222.23.163',
      dbname = 'stem_finder'
    ) # TODO: use a configuration group group = "my-db")
  return(con)
}  

conn <- getSqlConnection()

student_conn <- dbSendQuery(conn, "select * from Student")
student_locations <- dbFetch(student_conn)


coach_conn<-dbSendQuery(conn, "select * from coach")
coach_locations<-dbFetch(coach_conn)

team_conn<-dbSendQuery(conn, "select * from team")
team_locations <-dbFetch(team_conn)
team_locations <- add_column(team_locations, Looking_Mentors = sample(c("yes", "no"), size=nrow(team_locations), replace=T))

data(zipcode)
colnames(team_locations)[2] <- "zip"
team_locations <- join(team_locations, zipcode, by="zip")
team_locations <- team_locations[team_locations$Accepting == "yes",]

student_locations <- add_column(student_locations, zip = str_sub(student_locations$Address, -5, -1))
student_locations <- join(student_locations, zipcode, by="zip")

coach_locations <- add_column(coach_locations, zip = str_sub(coach_locations$Address, -5, -1))
coach_locations <- join(coach_locations, zipcode, by="zip")


ui <- fluidPage(theme = shinytheme("lumen"),
                useShinyjs(),
                titlePanel("stemLinked: FIRST LEGO League"),
                
                tabsetPanel(type = "tabs",
                            tabPanel("Find teams in your area",
                                     sidebarLayout(
                                       sidebarPanel("Please fill out the following information:",
                                                    textInput("student_age", "Age:", value = "", width = NULL,
                                                              placeholder = NULL),
                                                    textInput("student_address", "Address:", value = "", width = NULL,
                                                              placeholder = NULL),
                                                    radioButtons("distance", "How far are you willing to travel?", 
                                                                 choices = c("Within 5 miles", "Within 10 miles", "Within 25 miles", "Within 40 miles"), selected = NULL,
                                                                 inline = FALSE, width = NULL, choiceNames = NULL,
                                                                 choiceValues = NULL),
                                                    actionButton("student_search", "Enter"),
                                                    
                                                    br(), br(), br(),
                                                    h4("Legend:"),
                                                    p("Blue Pin with Home icon = Your location"),
                                                    p("Blue Pin with icon = Teams looking for students"),
                                                    br(), br(),
                                                    h4("Don't see any teams in your area? Press the button to send a request to available mentors"),
                                                    actionButton("help_request", "Submit a team formation request")),
                                       
                                       mainPanel("Map",   
                                                 leafletOutput("mymap", height = 700))
                                     )),
                            tabPanel("Find students or coaches in your area",
                                     sidebarLayout(
                                       sidebarPanel("Please fill out the following information:",
                                                    textInput("students_age", "Age:", value = "", width = NULL,
                                                              placeholder = NULL),
                                                    textInput("coach_address", "Address:", value = "", width = NULL,
                                                              placeholder = NULL),
                                                    radioButtons("coach_distance", "How far are you willing to travel?", 
                                                                 choices = c("Within 5 miles", "Within 10 miles", "Within 25 miles", "Within 40 miles"), selected = NULL,
                                                                 inline = FALSE, width = NULL, choiceNames = NULL,
                                                                 choiceValues = NULL),
                                                    actionButton("coach_search", "Enter"),
                                                    
                                                    br(), br(), br(),
                                                    h4("Legend:"),
                                                    p("Blue Pin with Home icon = Your location"),
                                                    p("Blue Pin with no icon = Students who are looking for a team"),
                                                    p("Purple Circle = Available Coaches"),
                                                    p("Green Pin = Teams looking for Coaches"),
                                                    p("Red Pin = Full Team")),
                                       
                                       mainPanel("Map",   
                                                 leafletOutput("coach_map", height = 700))
                                     )
                            )
                )
                
)
server <- function(input, output, session){
  
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng=-77.43, 
                 lat=37.54, popup="Your location")
  })
  
  
  observeEvent({
    input$student_search},
    {
      team_locs <- team_locations[team_locations$zip == str_sub(input$student_address, -5, -1),]
      lati <- street2coordinates(input$student_address)[[3]]
      longi <- street2coordinates(input$student_address)[[5]]
      dist <- switch(input$distance,
                     "Within 5 miles" = 8046,
                     "Within 10 miles" = 16093,
                     "Within 25 miles" = 40233,
                     "Within 40 miles" = 64374)
      if(dist == 8046){
        fix_zoom = 12
      }
      else if(dist == 16093){
        fix_zoom = 11
      }
      else if(dist == 40233){
        fix_zoom = 10
      }
      else if(dist == 64374){
        fix_zoom = 9
      }
      
      team_content <- paste(sep = "<br/>", 
                       paste0("Team Name:", team_locations$Team_Name),
                       paste0("Team Number:", team_locations$Team_Number),
                       paste0("Organization:", team_locations$Organization))
      
      
      leafletProxy("mymap") %>% clearMarkers() %>% clearShapes() %>%  
        setView(lat = lati, lng = longi, zoom = fix_zoom) %>% 
        addAwesomeMarkers(lat = lati, lng = longi, popup="Your location", icon = icon("home")) %>% 
        addCircles(lng = longi, lat = lati, weight = 1,
                   radius = dist) %>% 
        addMarkers(team_locations$longitude, team_locations$latitude, 
                   popup = team_content)
    })
  
  
  observeEvent({input$help_request}, {
    showModal(
      modalDialog(
        fluidPage(
          uiOutput("modalTitle"),
          textInput("new_student_first", "Please enter your first name:", value = "", width = NULL,
                    placeholder = NULL),
          textInput("new_student_last", "Please enter your last name:", value = "", width = NULL,
                    placeholder = NULL),
          textInput("new_student_age", "Please enter your age:", value = "", width = NULL,
                    placeholder = NULL),
          textInput("new_student_address", "Please enter your adress:", value = "", width = NULL,
                    placeholder = NULL),
          radioButtons("chosen_distance", "Maximum distance from you:", 
                       choices = c("Within 5 miles", "Within 10 miles", "Within 25 miles", "Within 40 miles"), selected = NULL,
                       inline = FALSE, width = NULL, choiceNames = NULL,
                       choiceValues = NULL),
          radioButtons("chosen_pronouns", "What are your preferred pronouns?", 
                       choices = c("She/Her/Hers", "He/Him/His", "They/Them/Theirs"), selected = NULL,
                       inline = FALSE),
          textInput("new_student_phone", "Please enter your phone number:", value = "", width = NULL,
                    placeholder = NULL),
          textInput("new_student_email", "Please enter your email:", value = "", width = NULL,
                    placeholder = NULL)
        ),
        footer = tagList(modalButton("Cancel"),
        actionButton("ok", "OK")),
        size="m", easyClose=TRUE
      )
    )
    
    
  })
  
  observeEvent({input$ok},
  if(!is.null(input$new_student_address) && str_sub(input$new_student_address, -5, -1) == "23060"){
    source_python("twilio-test.py")
  })
  
  # Coach Map
  output$coach_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng=-77.43, 
                 lat=37.54, popup="Your location")
  })
  
  observeEvent({
    input$coach_search},
    {
      # student_locs <- student_locations[student_locations$zip == str_sub(input$coach_address, -5, -1),]
      latitude <- street2coordinates(input$coach_address)[[3]]
      longitude <- street2coordinates(input$coach_address)[[5]]
      distance <- switch(input$coach_distance,
                     "Within 5 miles" = 8046,
                     "Within 10 miles" = 16093,
                     "Within 25 miles" = 40233,
                     "Within 40 miles" = 64374)
      if(distance == 8046){
        fixed_zoom = 12
        radii = 300
      }
      else if(distance == 16093){
        fixed_zoom = 11
        radii = 500
      }
      else if(distance == 40233){
        fixed_zoom = 10
        radii = 1000
      }
      else if(distance == 64374){
        fixed_zoom = 9
        radii = 1500
      }
      
      accepting_teams <- team_locations[team_locations$Looking_Mentors == "yes",]
      full_teams <- team_locations[team_locations$Looking_Mentors == "no",]
      student_content <- paste(sep = "<br/>", 
                       paste0("Student Name:", student_locations$First_Name),
                       paste0("Pronouns:", student_locations$Pronouns),
                       paste0("Contact:", student_locations$Phone))
      coach_content <- paste(sep = "<br/>", 
                       paste0("Coach Name:", coach_locations$First_Name, " ", coach_locations$Last_Name),
                       paste0("Pronouns:", coach_locations$Pronouns),
                       paste0("Contact:", coach_locations$Phone))
      
      coach_team_content <- paste(sep = "<br/>", 
                            paste0("Team Name:", accepting_teams$Team_Name),
                            paste0("Team Number:", accepting_teams$Team_Number),
                            paste0("Organization:", accepting_teams$Organization))
      
      
      coach_full_team_content <- paste(sep = "<br/>", 
                                  paste0("Team Name:", full_teams$Team_Name),
                                  paste0("Team Number:", full_teams$Team_Number),
                                  paste0("Organization:", full_teams$Organization))
      
      leafletProxy("coach_map") %>% clearMarkers() %>% clearShapes() %>%  
        setView(lat = latitude, lng = longitude, zoom = fixed_zoom) %>% 
        addAwesomeMarkers(lat = latitude, lng = longitude, popup="Your location", icon = icon("home")) %>% 
        addCircles(lng = longitude, lat = latitude, weight = 1,
                   radius = distance) %>% 
        addMarkers(student_locations$longitude, student_locations$latitude, popup = student_content) %>% 
        addCircles(coach_locations$longitude, coach_locations$latitude, weight = 1,
                   radius = radii, color = "purple", fillOpacity = 40, popup = coach_content) %>%
        addCircles(accepting_teams$longitude, accepting_teams$latitude, weight = 1,
                   radius = radii, color = "green", fillOpacity = 40, popup = coach_team_content) %>%
        addCircles(full_teams$longitude, full_teams$latitude, weight = 1,
                   radius = radii, color = "red", fillOpacity = 40, popup = coach_full_team_content)
    })
}
# popup = paste0("Name:" + student_locations$`Student Information`)
shinyApp(ui=ui, server=server)