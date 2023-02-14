pacman::p_load(spotifyr, tidyverse, factoextra, FactoMineR, cluster)

Sys.setenv(SPOTIFY_CLIENT_ID = 'da313eb7d34948459de44d4f9bc6c206')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'c91752d38cbf45e1b865c14dcad3a046')
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
informações_musicas<-map(saved_track$track.id , get_track_audio_features)%>%
  bind_rows()#banco com as informacoes

########
teste_lista= list()
tamanho<-length(saved_track$track.id)
for (i in 1:tamanho) {
  teste_lista[[i+1]] <-saved_track[[18]][[i]][[2]]
}
teste_tabela<-tibble(teste_lista)
####

ggplot(data) +
  aes(x = liveness, y = danceability) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  labs(title = "Liveness vs danceability") +
  theme_minimal()

ggplot(data) +
  aes(x = valence, y = energy, size = danceability) +
  geom_point(shape = "circle", colour = "#112446") +
  theme_minimal()


ggplot(data) +
  aes(x = danceability, y = valence) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_minimal()


###teste k-means
var_selecionadas<-informações_musicas%>%
  select(id, energy, valence, tempo)#seleciona variaveis para cluster

dado_kmean<-scale(var_selecionadas[-1])#escala menos id

fviz_nbclust(dado_kmean, kmeans, method = "gap_stat")+
  geom_vline(xintercept = 3, linetype = 2)#Visualiza quantidade de cluster

teste_k<-kmeans(var_selecionadas$energy, centers = 3)#CLusterizacao

fviz_cluster(teste_k, var_selecionadas[-1], ellipse.type = "t")#visualizar cluster

lista_k<-teste_k$cluster

tabela_k<-var_selecionadas%>%
  cbind(lista_k)
teste1<-left_join(tabela_k, saved_track, by="track.id")
tabela_k<-rename(tabela_k, track.id=id)

PCA(var_selecionadas[-1])

#adicionando as musicas na playlist
spotifyr::add_tracks_to_playlist("0KNaaiRKW1IEGP8Myd883y", uris = estilo1$id[20:40])
estilo1<-tabela_k%>%
  filter(lista_k==1)
estilo2<-tabela_k%>%
  filter(lista_k==2)
spotifyr::add_tracks_to_playlist("2rA7Z9QNahX6v52MAS0rnC", uris = estilo2$id[20:40])


# visualizando get_recommendation
teste<-get_recommendations(seed_tracks = "3YbnOI0laocMLOQ4FEfXFl" )
informações_teste<-map(teste$id , get_track_audio_features)%>%
  bind_rows()

plot(informações_teste$danceability)
plot(informações_teste$energy)
plot(informações_teste$speechiness)
plot(informações_teste$acousticness)
plot(informações_teste$instrumentalness)
q()


####Pensar em cluster diferentes, adicionar genero do artista principal no banco de dado