# webscraping para datos de instagram
# luis m. roldán a. 

# DESCARGA DE LIBRERÍAS
library(tidyverse)
library(stringr)
library(readr)
library(rvest)
library(dplyr)
library(tidyr)

# leer la base de datos de aliados inicial
base_aliados <- read.delim("base_aliados.txt", header=FALSE)

# BASE DE CORRESPONSALES
base_corresp<-read_excel("nombres_corresponsales_empresa.xlsx")

# CONSTRUIR LA BASE DE NOMBRES
nombres<-as.vector(as.data.frame(base_corresp[,1]))
nombres_url<-vector(length=length(nombres))
for (i in 1:nrow(nombres)){
  nombres_url[i]<-paste0("https://www.google.com.co/search?q=INSTAGRAM+OFICIAL ",nombres[i,1], " COLOMBIA")
}

# generar listas vacías
google<-list()
instagram<-list()
titulo_google<-list()
nombres_url<-stringr::str_replace_all(nombres_url, " ", "+")
datos<-list()


# CONSULTAR EN GOOGLE LAS BASES DE DATOS PERTINENTES (paso 1)
for (i in 1:length(nombres_url)){
  google[[nombres[i,1]]]<-as.character(read_html(nombres_url[i]))
  titulo_google[[nombres[i,1]]]<-paste0(str_match(google[[nombres[i,1]]], "https://www.instagram.com/\\s*(.*?)\\s*/")[[1]], "reels/")
  print(paste0("Número de url visitadas:", i))
  print(paste0("Última revisada:", nombres[i,1]))
  }

datos<-list()

for (i in 20:length(titulo_google)){
  if (is.na(titulo_google[[i]])){
    instagram[[nombres[i,1]]]<-"NO SE ENCONTRO INFORMACION"
  }else if (nchar(titulo_google[[i]])<30){
    instagram[[nombres[i,1]]]<-"PAGINA INTERNA"
  }
  else{
    instagram[[nombres[i,1]]]<-as.character(read_html(titulo_google[[i]], handle = curl::new_handle("useragent" = "Mozilla/5.0")))
    datos$nombre_cuenta[[nombres[i,1]]]<-str_match(instagram[[nombres[i,1]]], "\"full_name\":\\\"\\s*(.*?)\\s*\",")[,2]
    datos$direccion[[nombres[i,1]]]<-str_match(instagram[[nombres[i,1]]], "\"alternateName\":\\\"\\s*(.*?)\\s*\",")[,2]
    datos$tipo[[nombres[i,1]]]<-str_match(instagram[[nombres[i,1]]], "\"@type\":\\\"\\s*(.*?)\\s*\",")[,2]
    datos$descripcion[[nombres[i,1]]]<-str_match(instagram[[nombres[i,1]]], "\"description\":\\s*(.*?)\\s*\",")[,2]
    datos$posts[[nombres[i,1]]]<-str_match(instagram[[nombres[i,1]]], "Following, \\s*(.*?)\\s* Post")[,2]
    datos$seguidores[[nombres[i,1]]]<-str_match(instagram[[nombres[i,1]]], "\"edge_followed_by\\\":\\{\"count\":\\s*(.*?)\\s*\\}")[,2]
    datos$siguiendo[[nombres[i,1]]]<-str_match(instagram[[nombres[i,1]]], "edge_follow\\\":\\{\"count\":\\s*(.*?)\\s*\\},")[,2]
    datos$negocios_similares[[nombres[i,1]]]<-str_match_all(instagram[[nombres[i,1]]], "\"username\":\"\\s*(.*?)\\s*\"")[[1]][,2]
    datos$biografia[[nombres[i,1]]]<-str_match(instagram[[nombres[i,1]]], "\"biography\":\\s*(.*?)\\s*\",")[,2]
    datos$es_comercial[[nombres[i,1]]]<-str_match(instagram[[nombres[i,1]]], "\"is_business_account\":\\s*(.*?)\\s*,")[,2]
    datos$es_profesional[[nombres[i,1]]]<-str_match(instagram[[nombres[i,1]]], "\"is_professional_account\":\\s*(.*?)\\s*,")[,2]
    datos$unido_reciente[[nombres[i,1]]]<-str_match(instagram[[nombres[i,1]]], "\"is_joined_recently\":\\s*(.*?)\\s*,")[,2]
    datos$categoria_negocio[[nombres[i,1]]]<-str_match(instagram[[nombres[i,1]]], "\"business_category_name\":\\\"\\s*(.*?)\\s*\",")[,2]
    datos$es_verificada[[nombres[i,1]]]<-str_match(instagram[[nombres[i,1]]], "\"is_verified\":\\s*(.*?)\\s*,")[,2]
closeAllConnections()
print(paste0("Número de url visitadas:", i))
print(paste0("Última revisada:", nombres[i,1]))
  }
}

for (i in names(datos$nombre_cuenta)){
  datos$negocios_similares[[i]]<-paste(datos$negocios_similares[[i]], " ")
}

datos_aliados_directos<-datos
datos_aliados_directos$negocios_similares<-as.character(datos_aliados_directos$negocios_similares)
data_aliados_directos<-do.call(cbind.data.frame, datos_aliados_directos)
data_aliados_directos$posts<-as.numeric(stringr::str_replace_all(as.character(data_aliados_directos$posts), ",", ""))

# datos aliados potenciales

datos_p<-list()
google_p<-list()
instagram_p<-list()
titulo_google_p<-list()
nombres_url_p<-list()
datos_pot<-list()
nombres_p<-list()

for (i in 1:length(datos)){
  nombres_url_p[[datos[[1]][i]]]<-paste0("https://www.instagram.com/",as.character(unique(datos$negocios_similares[[i]])), "/reels/")
  nombres_p[[datos[[1]][i]]]<-as.character(unique(datos$negocios_similares[[i]]))
}

datos_pot_d<-list()
for (i in 1:length(datos)){
  datos_p<-list()
  datos_pot<-list()
  for (j in 1: min (10, length(nombres_url_p[[i]]))){
    instagram_p[[nombres_p[[i]][j]]]<-as.character(read_html(as.character(nombres_url_p[[i]][j]), handle = curl::new_handle("useragent" = "Mozilla/5.0")))
    datos_p$nombre_cuenta<-str_match(instagram_p[[nombres_p[[i]][j]]], "\"full_name\":\\\"\\s*(.*?)\\s*\",")[,2]
    datos_p$direccion<-str_match(instagram_p[[nombres_p[[i]][j]]], "\"alternateName\":\\\"\\s*(.*?)\\s*\",")[,2]
    datos_p$tipo<-str_match(instagram_p[[nombres_p[[i]][j]]], "\"@type\":\\\"\\s*(.*?)\\s*\",")[,2]
    datos_p$descripcion<-str_match(instagram_p[[nombres_p[[i]][j]]], "\"description\":\\s*(.*?)\\s*\",")[,2]
    datos_p$posts<-str_match(instagram_p[[nombres_p[[i]][j]]], "Following, \\s*(.*?)\\s* Post")[,2]
    datos_p$seguidores<-str_match(instagram_p[[nombres_p[[i]][j]]], "\"edge_followed_by\\\":\\{\"count\":\\s*(.*?)\\s*\\}")[,2]
    datos_p$siguiendo<-str_match(instagram_p[[nombres_p[[i]][j]]], "edge_follow\\\":\\{\"count\":\\s*(.*?)\\s*\\},")[,2]
    datos_p$negocios_similares[[j]]<-str_match_all(instagram_p[[nombres_p[[i]][j]]], "\"username\":\"\\s*(.*?)\\s*\"")[[1]][,2]
    datos_p$biografia<-str_match(instagram_p[[nombres_p[[i]][j]]], "\"biography\":\\s*(.*?)\\s*\",")[,2]
    datos_p$es_comercial<-str_match(instagram_p[[nombres_p[[i]][j]]], "\"is_business_account\":\\s*(.*?)\\s*,")[,2]
    datos_p$es_profesional<-str_match(instagram_p[[nombres_p[[i]][j]]], "\"is_professional_account\":\\s*(.*?)\\s*,")[,2]
    datos_p$unido_reciente<-str_match(instagram_p[[nombres_p[[i]][j]]], "\"is_joined_recently\":\\s*(.*?)\\s*,")[,2]
    datos_p$categoria_negocio<-str_match(instagram_p[[nombres_p[[i]][j]]], "\"business_category_name\":\\\"\\s*(.*?)\\s*\",")[,2]
    datos_p$es_verificada<-str_match(instagram_p[[nombres_p[[i]][j]]], "\"is_verified\":\\s*(.*?)\\s*,")[,2]
    datos_pot[[nombres_p[[i]][j]]]<-datos_p
    Sys.sleep(3)
    print(paste0("Número de url visitadas:", j))
    print(paste0("Última revisada:", nombres_p[[i]][j]))
  }
  datos_pot_d[[datos[[1]][i]]]<-datos_pot
  print(paste0("grupo", i))
  Sys.sleep(10)
}

datos_aliados_potenciales<-datos_pot_d
df_a_pot<-list()

for (i in 1:length (datos_aliados_potenciales)){
  basse<-list()
  for (j in 1:length(datos_aliados_potenciales[[i]])){
    datos_aliados_potenciales[[i]][[j]]$negocios_similares<-as.character(datos_aliados_potenciales[[i]][[j]]$negocios_similares[[1]])
    basse[[j]]<-do.call(cbind.data.frame, datos_aliados_potenciales[[i]][[j]])
    basse_df<-do.call(rbind, basse)
  }
  df_a_pot[[i]]<-basse_df
  df_a_pot[[i]]$referencia<-names(datos_aliados_potenciales)[i]
  df_a_pot_gen<-basse_df<-do.call(rbind, df_a_pot)
  rownames(df_a_pot_gen) <- NULL
}
df_a_pot_gen$posts<-stringr::str_replace_all(as.character(df_a_pot_gen$posts), ",", "")
df_a_pot_gen$posts<-as.numeric(stringr::str_replace_all(as.character(df_a_pot_gen$posts), "k", "000"))


xlsx::write.xlsx(df_a_pot_gen, "base_exploracion1.xlsx")



# ejercicio basado en distancias (no aplica por ahora)

# ddata<-dplyr::select(data, nombre_cuenta, posts, seguidores, siguiendo)
# 
# ddata2<-dplyr::select(data2, nombre_cuenta, posts, seguidores, siguiendo)
# 
# dist<-matrix(NA, nrow(ddata2), nrow(ddata))
# 
# for (i in 1:nrow(ddata2)){
#   for (j in 1:nrow(ddata)){
#     dist[i,j]<-dist(rbind(ddata2[i,], ddata[j,]))
#   }
# }
# dist<-as.data.frame(dist)
# for (i in 1:nrow(dist)){
#   dist$variable[i]<-as.character(ddata$nombre_cuenta)[order(dist[i,], decreasing = F)[1]]
# }
# 
# 
# data3<-cbind(data2, dist$variable)
# 
# 
