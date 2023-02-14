
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
library(countrycode)



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
  DTOutput("liked_songs")
  
  
)






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
        a(href="https://spotifycharts.com/regional", "Spotify charts", 
          target ="_blank"),
        em(strong("with")),
        a(href = "https://developer.spotify.com/discover/", "audio features", 
          target="_blank"),
        "(such as danceability, valence, energy, tempo, mode, key/pitch,
 and many more) in real-time—for each song, of each country, and of each date 
 that is currently available on Spotify. This app is developed especially for 
 users who want to gain insights into everyday music listening or want to build 
 an individual database or large datasets for research in music psychology or 
 computational musicology or even for cross-cultural research while benefiting 
        from the perks of Spotify’s",
        a(href = "https://developer.spotify.com/",
          "API", target = "_blank"),
        "without writing hundreds of code lines.
 Retrieved data can be downloaded as a CSV file and can easily be
 imported into any statistical computing software—e.g.,",
        a(href = "https://jasp-stats.org/about/",
          "JASP", target = "_blank"),
        "or",
        a(href = "https://www.ibm.com/products/spss-statistics", "SPSS.", 
          target = "_blank"),
        "Additionally, some basic data visualizations provide first insights
into the data structure. All in all, the",
        strong("Spotify Charts Scraper"),
        "makes music streaming data retrieval more accessible:
It’s easy to use, and it’s open-source. Feel free to browse the source code on",
        a(href = "https://github.com/KewKalustian/Shiny_Spotify_Charts_Scraper",
          "GitHub,", target = "_blank"),
        "where you can also open an issue or even do a pull request."),
      p(
        style = "text-align: justify; font-size: 14pt;",
        "Since this app relies on Spotify’s metadata, it is currently 
mandatory that users must be registered by the developer to receive their log-in 
credentials via e-mail. This is necessary in order to comply with the",
        a(
          href = "https://developer.spotify.com/policy/",
          "Spotify Developer Policy.",
          target = "_blank"
        ),
        "Those credentials are stored encrypted—a hashing algorithm protects 
from brute-force attacks. If users do not want to be registered anymore, they 
have to inform the developer accordingly as unambiguously as possible so they 
can be de-registered with this app. Their user data and their received 
credentials will then be deleted."
      ),
      p(
        style = "text-align: justify; font-size: 14pt;",
        "Once a bigger audience has been onboarded, it is intended to 
facilitate access to this app—this depends ultimately on Spotify’s approval."
      ),
      br(),
      tags$blockquote(
        p(
          "Special thanks to",
          a(href = "https://developer.spotify.com/", "Spotify", 
            target = "_blank"),
          "for making it possible to retrieve their metadata by using their 
            developer API, to",
          a(href = "https://github.com/klausfrieler" , "Klaus Frieler", 
            target = "_blank"),
          "from the",
          a(
            href = "https://www.aesthetics.mpg.de/en.html",
            "MPI for Empirical
              Aesthetics,",
            target = "_blank"
          ),
          "and to the",
          a(
            href = "http://music-psychology.org/index.html",
            "German Society
              for Music Psychology (DGM),",
            target = "_blank"
          ),
          "who agreed to host this app on
            their server."
        )
      ),
      p("Please look forward to future updates and stay tuned!"),
      hr(),
      h5("Recommended Browser: Google Chrome"),
      h5("Recommended Minimum Display Size: 24-inch" ),
      hr(),
      h6(
        "The linked sites are not under the control of the developer. The
developer is also not responsible for the contents of any linked website.
Those links are provided as a convenience only and shall not be construed as an
endorsement of, sponsorship of, or as affiliated with the linked website by the
developer—unless it is explicitly stated."
      ),
      footer = modalButton("OK"),
      size = "l",
      easyClose = T
    ))
  })
  
  #dataset_ls = data.frame(Aguardando = c("Preencha suas credenciais e dê PLAY!"))
  
  dataset_ls = eventReactive(input$go,{
    
    data = tryCatch({
      pacman::p_load(spotifyr, tidyverse, factoextra, FactoMineR, cluster)
      Sys.setenv(SPOTIFY_CLIENT_ID = input$ID)#'da313eb7d34948459de44d4f9bc6c206'
      Sys.setenv(SPOTIFY_CLIENT_SECRET = input$secret )#'c91752d38cbf45e1b865c14dcad3a046'
      access_token <- get_spotify_access_token()
      lista = list()
      for (i in seq(0,10)){
        x = i*50
        lista[[i+1]] <- get_my_saved_tracks(limit = 50, offset = x)
        if (nrow(lista[[i+1]]) < 50){
          break
        }
      }#obtendo as musicas
      saved_track <- bind_rows(lista, .id = "column_label")
      saved_track
    }, error = function(e) {
      "erro na inserção de atributos"
    })
    
    data = as.tibble(data)
    informações_musicas<-map(data$track.id , get_track_audio_features)%>%
      bind_rows()
    informações_musicas<-informações_musicas%>%
      select(id, energy, valence, tempo)
    data=data%>%
      select(track.name,track.album.name, track.popularity, track.id)%>%
      rename(Name=track.name, Album= track.album.name, Popularity=track.popularity, id=track.id)
    teste1<-left_join(data, informações_musicas, by="id")
    teste1
  })
  

  output$liked_songs = renderDT({dataset_ls()})
  
}

#####################
#### D E P L O Y ####
#####################

shinyApp(ui = ui, server = server)



head(get_my_top_artists_or_tracks())