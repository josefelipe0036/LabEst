
library(shiny)
library(shinydashboard)   
library(shinydashboardPlus)
library(shinyauthr)
library(shinybusy)
library(shinycssloaders)
library(shinyjs)
library(DT)
library(RSQLite)
library(DBI)
library(sodium)
library(htmlwidgets)
library(tidyverse)
library(rvest)
library(lubridate)
library(magrittr)
library(spotifyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(GGally)



Imp <- function() {
  fluidPage(
    br(),
    hr(),
    a(
      href = "",
      h6(strong("Developed and built by LabEst")),
      target = "_blank",
      style = "display: block; color: white"
    ),
    br(),
    br(),
    strong("Powered by"),
    br(),
    br(),
    a(
      href = c("https://developer.spotify.com/"),
      img(src = "https://raw.githubusercontent.com/KewKalustian/spotify_charts_scraper/master/img/Spotify_Logo_CMYK_White.png",
          width = "50%"),
      target = "_blank"
    ),
    hr(),
      br(),
      h6(strong("teste")),
      style = "display: block; color: white",
      target = "_blank",
    hr(),
    a(
      href = c("https://www.aesthetics.mpg.de/en.html"),
      target = "_blank"
    )
  )
}

# H E A D E R

header <- dashboardHeader(
  title = "Spotify nome do app",
  titleWidth = 300,
  tags$li(
    class = "dropdown",
    a(
      icon("github"),
      href = "https://github.com",
      title = "Browse Source Code on GitHub",
      target = "_blank",
      style = "font-size: 25px; display: contents"
    )
  ),
  
  tags$li(
    class = "dropdown",
    style = "padding: 8px"
  )
)


sidebar <- dashboardSidebar(
  width = 290,
  sidebarMenu(
    
    # Removing the sidebar toggle element
    tags$script(
      JS(
        "document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"
      )
    ),
    
    br(),
    br(),
    br(),
    
    br(),
    br(),
    tags$style(
      HTML(
        ".shiny-notification {position:fixed; top: 35%; left: 283px; right: 0px;
         width: 270px; background-color: #283747; color: white}"
      )
    ),
    textInput("user", "insira o username"),
    textInput("ID","Insira o ID"),
    textInput("secret", "insira o secret"),
    textOutput("text_out")
   ,
    br(),
    br(),
   
    
    actionButton(
      "go",
      label = div("APP!", icon("play-circle")),
      width = 270,
      style = "color: white; font-size: 32px; font-weight: bold; text-align:
               center; background-color: #283747;
               display: contents;"
    ),
    
    Imp()
  )
)

# B O D Y
body <-  dashboardBody(
  tags$head(tags$style(
    HTML(
      "
            /* logo */
           .main-header .logo {font-weight: bold;
           font-size: 22px}
           .skin-blue .main-header .logo {
           background-color:#283747}
           
           /* logo when hovered */
           .skin-blue .main-header .logo:hover {
           background-color: #283747}
           
           /* rest */
           .skin-blue .main-header .navbar-static-top {
           background-color: #283747}
           
           /* main sidebar */
           .skin-blue .main-sidebar {
           background-color: #283747}
           
           /* links + when links hovered*/
           a {color: #F47920}
           a:hover {color: #283747 }
           
           /* buttons/icons + when buttons/icons hovered */
           .fa-download:hover,
           .fa-play-circle:hover ,
           .fa-info-circle:hover ,
           .fa-github:hover {color: #F47920}
  
           #login-button {background-color: #283747; border: none}
           #login-button:hover {background-color: #F47920}
           #logout-button {background-color: #8b0000; border: none}
           #logout-button:hover {background-color:#6f0000}
           
           /* github icon position */
           .fa-github {padding-top: 12px; padding-bottom: 10px;
           padding-right:10px}
           
           /* login panel / body */
           #rmve .with-border {display:none}
           #rmve .box-body  {background-color: #283747}
           h6:nth-child(1):hover, br+ h6 strong:hover  {color: #F47920}
          
      "
    )
  )),
  
 
      )
  
  box(
    id = "table", width = 12, 
    title =  p(
      tags$style(
        HTML(
          type = "text/css",
          "#help {display: contents; color: #283747} #downloadData
          {background-color: white; color: #283747; border-style: none;
          position: absolute; right:0px; top:1px; padding:4px}"
        )
      )))
  

  
 

  
ui <- dashboardPage(header, sidebar, body)


#####################
#### S E R V E R ####
#####################


server <- function(input, output, session) {
  
  observeEvent(once = T, session, {
    showModal(modalDialog(  
      title = p(
        style = "text-align: center; font-size: 42px; font-weight:bold",
        "Nome do APP",
        br(),
        p(style = "font-weight:normal; font-size: 21px; text-align: center",
          "v0.2.6")
      ),
      h2('Primeiros passos'),
      
      style = "text-align: justify; font-size: 14pt;",
      p("The",
        strong("NOME DO APP"),
        "is a",
        a(href = "https://shiny.rstudio.com/", "Shiny application", 
          target = "_blank"),
        "to retrieve daily",
        a(href="https://spotifycharts.com/regional", "Spotify charts o nome do site onde os dados foram pegado", 
          target ="_blank"),
        em(strong("with")),
        a(href = "https://developer.spotify.com/discover/", "audio features", 
          target="_blank"),
        "o app",
        
        strong("Spotify teste"),
        a(href = "https://github.com",
          "GitHub,", target = "_blank"),
        "teste"),
      p(
        style = "text-align: justify; font-size: 14pt;",
        " ccccccccccc",
        a(
          href = "https://developer.spotify.com/policy/",
          "Spotify Developer Policy.",
          target = "_blank"
        ),
        "teste #######"
      ),
      p(
        style = "text-align: justify; font-size: 14pt;",
        ""
      ),
      br(),
      tags$blockquote(
        p(
          "Special thanks to",
          a(href = "https://developer.spotify.com/", "Spotify", 
            target = "_blank"),
          a(href = "https://github.com" , " ", 
            target = "_blank"),
          "from the",
          a(
            href = "https://www.aesthetics.mpg.de/en.html",
            "MPI for Empirical
              Aesthetics,",
            target = "_blank"
          )
        )
      ),
      p("Please look forward to future updates and stay tuned!"),
      hr(),
      h5("Recommended Browser: Google Chrome"),
      h5("Recommended Minimum Display Size: 24-inch" ),
      hr(),
      h6(
        "Teste"
      ),
      footer = modalButton("OK"),
      size = "l",
      easyClose = T
    ))
  })
  
  
  
  
}

#####################
#### D E P L O Y ####
#####################

shinyApp(ui = ui, server = server)


