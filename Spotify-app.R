#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rm(list = ls())
library(shinydashboard)
library(shiny)
library(ggplot2)



# Define header
header = dashboardHeader(title = "Spotify Gilian")

# Define sidebar
sidebar = dashboardSidebar()

body  = dashboardBody(h2("My (Spotify) music taste & recommendations"),
                      h3("Based on my long term Spotify behavior"),
                      fluidRow(box(title = "Me vs. Top 40", plotOutput("firstspinner"), solidHeader = TRUE, status = "danger"),box(title = "Recommendations", dataTableOutput('table'), solidHeader = TRUE, status = "danger")),
                      h3("Based on my short term Spotify behavior"),
                      fluidRow(box(title = "Me vs. Top 40", plotOutput("shortterm_plot"), solidHeader = TRUE, status = "danger"),box(title = "Recommendations", dataTableOutput('shortterm_recommendations'), solidHeader = TRUE, status = "danger"))
                      )

ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(spotifyr)
  library(tidyverse)
  library(knitr)
  library(httpuv)
  Sys.setenv(SPOTIFY_CLIENT_ID = '') # provide your Client ID
  Sys.setenv(SPOTIFY_CLIENT_SECRET = '') # provide your Client SECRET
  access_token <- get_spotify_access_token()
  
  top_artists_long_time = spotifyr::get_my_top_artists_or_tracks(type = "artists",time_range = 'long_term', limit = 50)
  top_tracks_long_time = spotifyr::get_my_top_artists_or_tracks(type = "tracks",time_range = 'long_term', limit = 50)
  
  audio_data = c()
  for (i in 1:50){
    print(i)
    audio = get_track_audio_features(top_tracks_long_time$id[i])
    audio_data = rbind(audio_data, audio)
  }
  audio_data = audio_data %>% mutate(duration_s = duration_ms/1000, duration_min = duration_s/60)
  audio_data = audio_data %>% pivot_longer(cols = c(1:11,17,19,20))
  audio_data$playlist = "my music taste"
  
  top_40_tracks = get_playlist_tracks("5lH9NjOeJvctAO92ZrKQNB")
  
  audio_data_top_40 = c()
  for (i in 1:length(top_40_tracks)){
    #print(i)
    audio_40 = get_track_audio_features(top_40_tracks$track.id[i])
    audio_data_top_40 = rbind(audio_data_top_40, audio_40)
  }
  
  audio_data_top_40 = audio_data_top_40 %>% mutate(duration_s = duration_ms/1000, duration_min = duration_s/60)
  audio_data_top_40 = audio_data_top_40 %>% pivot_longer(cols = c(1:11,17,19,20)) 
  audio_data_top_40$playlist = "top 40"
  
  compare_me_vs_top_40 = bind_rows(audio_data, audio_data_top_40) 
  compare_me_vs_top_40$playlist = factor(compare_me_vs_top_40$playlist, levels= c("top 40", "my music taste"))
  means = compare_me_vs_top_40 %>% filter(playlist == "my music taste") %>% filter(name != "duration_s") %>% filter(name != "duration_ms") %>% group_by(name) %>%
    summarize(mean = mean(value))
  
  # long term recommendation
  access_token <- get_spotify_access_token()
  get_spotify_authorization_code()
  #target_mode = as.numeric(filter(means, name == "mode")[2])
  target_loudness = as.numeric(filter(means, name == "loudness")[2])
  target_liveness = as.numeric(filter(means, name == "liveness")[2])
  target_energy = as.numeric(filter(means, name == "energy")[2])
  #target_key= as.numeric(filter(means, name == "key")[2])
  target_instrumentalness = as.numeric(filter(means, name == "instrumentalness")[2])
  target_acousticness = as.numeric(filter(means, name == "acousticness")[2])
  target_danceability = as.numeric(filter(means, name == "danceability")[2])
 
  # recommendations
  recommendations_long_term = get_recommendations(seed_artists = top_artists_long_time$id[c(sample(c(1:length(top_artists_long_time)), size = 5))], target_loudness = target_loudness, target_liveness = target_liveness, target_energy = target_energy, target_instrumentalness = target_instrumentalness, target_acousticness = target_acousticness, target_danceability = target_danceability,  limit = 50) %>% select(name,popularity,external_urls.spotify)
  
  
  ###########################
  ## short term #############
  ###########################
  
  top_artists_short_term = spotifyr::get_my_top_artists_or_tracks(type = "artists",time_range = 'short_term', limit = 50) 
  top_tracks_short_term = spotifyr::get_my_top_artists_or_tracks(type = "tracks",time_range = 'short_term', limit = 50)
  
  audio_data = c()
  for (i in 1:50){
    print(i)
    audio = get_track_audio_features(top_tracks_short_term$id[i])
    audio_data = rbind(audio_data, audio)
  }
  audio_data = audio_data %>% mutate(duration_s = duration_ms/1000, duration_min = duration_s/60)
  audio_data = audio_data %>% pivot_longer(cols = c(1:11,17,19,20))
  audio_data$playlist = "my music taste"
  
  compare_me_vs_top_40_short_term = bind_rows(audio_data, audio_data_top_40) 
  compare_me_vs_top_40_short_term$playlist = factor(compare_me_vs_top_40$playlist, levels= c("top 40", "my music taste"))
  
  means = compare_me_vs_top_40_short_term %>% filter(playlist == "my music taste") %>% filter(name != "duration_s") %>% filter(name != "duration_ms") %>% group_by(name) %>%
    summarize(mean = mean(value))
  
  # long term recommendation
  access_token <- get_spotify_access_token()
  get_spotify_authorization_code()
  #target_mode = as.numeric(filter(means, name == "mode")[2])
  target_loudness = as.numeric(filter(means, name == "loudness")[2])
  target_liveness = as.numeric(filter(means, name == "liveness")[2])
  target_energy = as.numeric(filter(means, name == "energy")[2])
  #target_key= as.numeric(filter(means, name == "key")[2])
  target_instrumentalness = as.numeric(filter(means, name == "instrumentalness")[2])
  target_acousticness = as.numeric(filter(means, name == "acousticness")[2])
  target_danceability = as.numeric(filter(means, name == "danceability")[2])
  
  
  # recommendations
  recommendations_short_term = get_recommendations(seed_artists = top_artists_short_term$id[c(sample(c(1:length(top_artists_short_term)), size = 5))], target_loudness = target_loudness, target_liveness = target_liveness, target_energy = target_energy, target_instrumentalness = target_instrumentalness, target_acousticness = target_acousticness, target_danceability = target_danceability,  limit = 50) %>% select(name,popularity,external_urls.spotify)
  
  
  output$firstspinner <- renderPlot({
    compare_me_vs_top_40 %>% ggplot(aes(y = value, fill = playlist)) + geom_density(alpha = 0.4) + coord_flip() + facet_wrap(~name, scales = "free") + theme_minimal() + xlab("") + ggtitle("Long term music taste") + scale_fill_manual(values = c("black", "red"))
    })
  output$table = renderDataTable({recommendations_long_term}, options = list(pageLength = 10))
  
  
  output$shortterm_plot <- renderPlot({
    compare_me_vs_top_40_short_term %>% ggplot(aes(y = value, fill = playlist)) + geom_density(alpha = 0.4) + coord_flip() + facet_wrap(~name, scales = "free") + theme_minimal() + xlab("") + ggtitle("Short term music taste") + scale_fill_manual(values = c("black", "red"))
  })
  output$shortterm_recommendations = renderDataTable({recommendations_short_term}, options = list(pageLength = 10))
}

# Run the application 
shinyApp(ui = ui, server = server)

