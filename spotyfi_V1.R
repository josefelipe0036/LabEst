pacman::p_load(spotifyr, tidyverse)

Sys.setenv(SPOTIFY_CLIENT_ID = 'da313eb7d34xxxxxxxxxxxxxxx06')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'x2xxxxxxxxxxxxxxxxxxxxeaa9dd9dcf9')

access_token <- get_spotify_access_token()
lista = list()

for (i in seq(0,10)){
  x = i*50
  
  lista[[i+1]] <- get_my_saved_tracks(limit = 50, offset = x)
  
  if (nrow(lista[[i+1]]) < 50){
    break
  }
}
saved_track <- bind_rows(lista, .id = "column_label")



cl <- parallel::makePSOCKcluster(3) # cria as cópias do R que rodam em paralelo
doParallel::registerDoParallel(cl) 
musicas<-map(meus_albuns$track.id, get_track)





#########
teste2<-map(saved_track$track.id , get_track_audio_features)

teste3<-bind_rows(teste2)




teste<-as.data.frame(do.call(cbind, musica))
get_track(meus_albuns$track.album.id[1])
for (i in aa) {
  f<- bind_rows(aa[[]])
  
}
