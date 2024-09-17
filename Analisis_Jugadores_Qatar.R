# Se llaman las librerias

library(dplyr)
library(readxl)
library(tidyverse)
library(knitr)
library(ggplot2)
library(plotly)
library(ggpubr)

# La ruta de la carpeta donde se trabaja

setwd("C:/LAURA/MASTER FUTBOL/R")

# Se importa la base de datos
Qatar=read_excel("Qatar_2022_jugadores.xlsx")

names(Qatar) = c("SELECCION","PARTIDO","Dorsal","Name","Surname","Position","Total_Dist","Total_Dist_rel",
                 "Dist_0.7","Dist_7.15","Dist_15.20","Dist_20.25","Dist_25","HSR_n","Sprints_n",
                 "Vmax","rival","result_cual","Formacion","MIN_Jugado","goles_favor","goles_contra",
                 "posesion","Grupo","Jornada","TIEMPO_TOTAL")

names(Qatar)

# EJERCICIO 1 

{
  # Obtenga el top 5 de jugadores con más Total_dist y en qué partido fue
  
  top_5_dist= Qatar %>% select(SELECCION, PARTIDO, Name, Total_Distance)
  
  kable(top_5_dist[order(-top_5_dist$Total_Dist),]%>% slice_head(n=5))
}

# EJERCICIO 2 

{
  # Obtenga el top 5 de jugadores con más Distance_25 y en qué partido fue
  
  top_5_dist_25= Qatar %>% select(SELECCION, PARTIDO, Name, Distance_25)
  
  kable(top_5_dist_25[order(-top_5_dist_25$Dist_25),]%>% slice_head(n=5))
}

# EJERCICO 3

{
  #Obtenga el top 10 de jugadores más rápidos del mundial
  
  top_10_v= Qatar %>% select(SELECCION, PARTIDO, Name, Vmax)
  
  kable(top_10_v[order(-top_10_v$Vmax),]%>% slice_head(n=10))
}

# EJERCICO 4

{
  # En el fútbol se utiliza como KPI la variable HSR (High speed running). Calcule una nueva 
  # variable y llámela HSR sabiendo que HSR = Distancia recorrida a + 20 km/h
  
  # Para calcular esta variable habrá que sumar la distancia en el  rango de velocidad entre 20 y 25 km/h y 
  # la distancia en el rango de velocidad a más de 25 km/h
  
  Qatar1 = Qatar # se crea una nueva base de datos por si la cagamos
  
  Qatar1$HSR = Qatar1$Dist_20.25+Qatar1$Dist_25
  
  colnames(Qatar)
  
  Qatar1 = Qatar1[,c("SELECCION","PARTIDO","Dorsal","Name","Surname","Position","Total_Dist",
                       "Total_Dist_rel", "Dist_0.7","Dist_7.15","Dist_15.20",
                       "Dist_20.25","Dist_25","HSR","HSR_n","Sprints_n","Vmax","rival","result_cual",
                       "Formacion","MIN_Jugado","goles_favor","goles_contra","posesion","Grupo",
                       "Jornada","TIEMPO_TOTAL")]
  Qatar1
}

# EJERCICO 5

{
  # ¿Deberíamos tener en cuenta el tiempo? Posiblemente un jugador que haya jugado 
  # 100 minutos haya recorrido más distancia que uno que haya jugado 80. Calcule sobre 
  # todas las variables de distancia, HSR_n y Sprint_n el relativo al tiempo de juego 
  # (de cada jugador)
  
  
  Qatar2=Qatar[,1:15]
  Qatar2
  Qatar2=subset(Qatar2, select=-Total_Dist_rel)
  tiempo= Qatar %>% select(MIN_Jugado)
  
  Qatar2_1=Qatar2[,1:6]
  Qatar2_2=Qatar2[,7:14]
  
  for (i in names(Qatar2_2)) {
    Qatar2_2[[i]]=Qatar2_2[[i]]/Qatar$MIN_Jugado
  }
  
  
  names(Qatar2_2) =c("Total_Dist_rel","Dist_0.7_rel","Dist_7.15_rel","Distance_15.20_rel",
                       "Diste_20.25_rel","Dist_25_rel","HSR_n_rel","Sprints_n_rel")
  
  Qatar3=cbind(Qatar2_1,Qatar2_2)

}

# EJERCICO 6

{
  #  Teniendo en cuenta solo la fase de grupos: obtenga el top 5 de jugadores con más 
  # distancias acumuladas en los 3 partidos, para cada posición en la variable calculada 
  # HSR y Total_dist_m
  
  # Primero hay que ver cuantos grupos había en este mundial
  
  unique(Qatar1$Grupo)
  
  # A continuación se flitra la fase de grupos
  
  grupos= Qatar1 %>% filter(Grupo %in% c("A","B","C","D",
                                         "E", "F","G", "H"))
  
  # Ya se puede calcular
  
  distancias_acumuladas = grupos %>% group_by(Name, Position, Grupo) %>%
    summarise(
      HSR= sum(HSR, na.rm = TRUE),
      Total_Dist = sum(Total_Dist, na.rm = TRUE)
    ) %>% ungroup()
  
  # Y se calcula el top 5 de cada una de las variables
  
  top_5_HSR = distancias_acumuladas %>% group_by(Position, Grupo) %>%
    arrange(Grupo,Position, desc(HSR)) %>% slice_head(n=5)
  
  
  top_5_Dist = distancias_acumuladas %>% group_by(Position, Grupo) %>%
    arrange(Grupo,Position, desc(Total_Dist)) %>% slice_head(n=5)
  
  # Esto se tiene que añadir porque sino a la hora de elegir el grupo no salen ordenados
  
  # Crear un marco de datos con todas las combinaciones posibles de Grupo y Position
  
  all_combinations = expand.grid(
    Position = c("POR", "DFC", "LAT", "CAR", "MCD", "MC", "MCO", "INT", "EXT", "DC"),
    Grupo = sort(unique(top_5_Dist$Grupo))
  )
  
  # Combinar con los datos reales
  
  top_5_Dist = right_join(all_combinations, top_5_Dist, by = c("Position", "Grupo"))
  
  # Ordenar los niveles de Grupo
  
  top_5_Dist$Grupo = factor(top_5_Dist$Grupo, levels = sort(unique(top_5_Dist$Grupo)))
  
  group = unique(top_5_Dist$Grupo)
  
  # Crear un plot por cada grupo y guardarlo
  for(grupo in group) {
    # Filtrar los datos para el grupo actual
    data_grupo = subset(top_5_Dist, Grupo == grupo)
    
    data_grupo$Position=factor(data_grupo$Position, levels = c("POR", "DFC", "LAT",
                                                               "CAR", "MCD", "MC",
                                                               "MCO", "INT", "EXT",
                                                               "DC"))
    
    # Crear el plot
    p = ggplot(data_grupo, aes(Name, Total_Dist, fill=Position)) +
      geom_col(show.legend = FALSE) +
      labs(y = "Distancia Total", x="Nombre") +
      facet_wrap(~ Position, scales = "free_y") +
      coord_flip() +
      scale_y_continuous(expand = c(0, 0)) +
      ggtitle(paste("Grupo:", grupo))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p)
  }
  
  
  # Crear un marco de datos con todas las combinaciones posibles de Grupo y Position
  
  all_combinations <- expand.grid(
    Position = c("POR", "DFC", "LAT", "CAR", "MCD", "MC", "MCO", "INT", "EXT", "DC"),
    Grupo = sort(unique(top_5_HSR$Grupo))
  )
  
  # Combinar con los datos reales
  
  top_5_HSR = right_join(all_combinations, top_5_HSR, by = c("Position", "Grupo"))
  
  # Ordenar los niveles de Grupo
  
  top_5_HSR$Grupo = factor(top_5_HSR$Grupo, levels = sort(unique(top_5_HSR$Grupo)))
  
  group = unique(top_5_HSR$Grupo)
  
  # Crear un plot por cada grupo y guardarlo
  for(grupo in group) {
    # Filtrar los datos para el grupo actual
    data_grupo = subset(top_5_HSR, Grupo == grupo)
    
    data_grupo$Position=factor(data_grupo$Position, levels = c("POR", "DFC", "LAT",
                                                               "CAR", "MCD", "MC",
                                                               "MCO", "INT", "EXT",
                                                               "DC"))
    
    # Crear el plot
    p = ggplot(data_grupo, aes(Name, HSR, fill=Position)) +
      geom_col(show.legend = FALSE) +
      labs(y = "HSR", x="Nombre") +
      facet_wrap(~ Position, scales = "free_y") +
      coord_flip() +
      scale_y_continuous(expand = c(0, 0)) +
      ggtitle(paste("Grupo:", grupo))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p)
    
  }
  
  
}

# EJERCICO 7

{
  # Seleccione las observaciones de los 20 jugadores con +HSR (en un partido) y haga un 
  # diagrama de dispersión situando el HSR en el ejeX y la vmax en el ejeY 
  
  top_20_HSR = Qatar1 %>% arrange(desc(HSR)) %>% slice_head(n=20)
  
  ggplot(top_20_HSR, aes(x=HSR, y=Vmax))+
    geom_point()+
    geom_text(aes(label=Name), hjust=1, vjust=0.5, size = 3, nudge_x = -1.6)
}

# EJERCICO 8

{
  # Eliminando porteros y jugadores con <60 minutos jugados, obtenga para todas las 
  # variables de rendimiento físico el promedio de cada selección en cada partido 
  
  # En primer lugar se filtra para eliminar a los porteros y solo escoger los jugadores con + 60 min
  
  Qatar_4 = Qatar1 %>% filter(Position != "POR" & MIN_Jugado>=60)
  
  # Hay que distinguir en cada partido cada una de las selecciones
  
  Partidos = Qatar_4 %>%select(PARTIDO, SELECCION) %>% distinct()
  
  # Se crea un dataframe para albergar los datos promedios
  
  promedio_partido = data.frame()
  
  # Se realiza el bucle
  
  for(i in 1:nrow(Partidos)){
    Seleccion=Partidos[["SELECCION"]][i]
    Partido=Partidos[["PARTIDO"]][i]
    
    DATA= Qatar_4 %>% filter(PARTIDO==Partido & SELECCION==Seleccion)
    
    DATA= DATA %>% select_if(is.numeric)
    DATA = t(colMeans(DATA))

    Info=data.frame(SELECCION=Seleccion, PARTIDO=Partido)
    
    DATA=cbind(DATA,Info)
    
    promedio_partido=rbind(promedio_partido, DATA)
  }
  
  # Añado manualmente result_cual porque en el bucle lo elimino al no ser numérico
  # además al hacerlo como la seleccion y partido obtengo error
  
  promedio_partido = promedio_partido %>% mutate(result_cual = case_when(
    goles_favor > goles_contra ~ "Victoria",
    goles_favor < goles_contra ~ "Derrota",
    goles_favor == goles_contra ~ "Empate"
  ))
  
  # Ahora se enseña el resultado en una tabla
  
  kable(promedio_partido %>% slice_head(n=5))
  
}

# EJERCICO 9

{
  # ¿Qué selección recorrió más Distancia total y en qué partido? ¿Cuál fue su result_cual? 
  
  # Ahora se ordena por orden descendente Total_Distance y se coge el más alto
  
  top_TD = promedio_partido %>% arrange(desc(Total_Dist)) %>% slice_head(n=1)
  
  # Se presenta como  una tabla las variables que considero apropiadas
  
  kable(head(top_TD %>% select(Total_Dist,SELECCION, PARTIDO, result_cual)))
  
}

# EJERCICO 10

{
  # ¿Qué selección tuvo más HSR_n en relativo y en qué partido fue? 
  
  # El calculo es similar al apartado anterior
  
  top_HSRn = promedio_partido %>% arrange(desc(HSR_n)) %>% slice_head(n=1)
  
  # Se presenta como  una tabla las variables que considero apropiadas
  
  kable(head(top_HSRn %>% select(HSR_n,SELECCION, PARTIDO, result_cual)))
}

# EJERCICO 11

{
  # En valor acumulado de partido, ¿qué selección realizó más distancia a + 25 km/h en la 
  # final del mundial? ¿Qué jugador fue el que más distancia total recorrió? 
  
  
  # En este caso se entiende que hay que volver a la base de datos Qatar1 y obtener los valores acumulados
  # de la final
  
  final = Qatar1%>% filter(Jornada == "Final")
  final_p = final %>%select(PARTIDO, SELECCION) %>% distinct()
  
  # Se crea un dataframe para albergar los datos promedios
  
  acumulado_partido = data.frame()
  
  # Se realiza el bucle
  
  for(i in 1:nrow(final_p)){
    Seleccion=final_p[["SELECCION"]][i]
    Partido=final_p[["PARTIDO"]][i]
    
    DATA= final %>% filter(PARTIDO==Partido & SELECCION==Seleccion)
    
    DATA= DATA %>% select_if(is.numeric)
    DATA = t(colMeans(DATA))
    
    Info=data.frame(SELECCION=Seleccion, PARTIDO=Partido)
    
    DATA=cbind(DATA,Info)
    
    acumulado_partido=rbind(acumulado_partido, DATA)
  }
  
  # Ahora se muestra la selección con mas distancia a +25 km/h
  
  top_25f = acumulado_partido  %>% arrange(desc(Dist_25)) %>% slice_head(n=1)
  
  kable(head(top_25f %>% select(Dist_25,SELECCION, PARTIDO)))
  
  # Ahora se muestra cual fue el jugador con mas distancia recorrida
  
  top_distf = final %>% arrange(desc(Total_Dist)) %>% slice_head(n=1)
  
  kable(head(top_distf %>% select(Total_Dist,Name,SELECCION, PARTIDO)))

}

# EJERCICO 12

{
  ## Cree un data frame “Prom_seleccion_grupo” eliminando porteros, obtenga el 
  # promedio de cada selección en la fase de grupos en la variable HSR y posesión y 
  # represénte con barras el HSR de las selecciones
  
  # En primer lugar se quita a los porteros y los partidos que no pertenecen a la fase de grupos
  
  fase_grupos = Qatar1 %>% filter(Position != "POR" & !Grupo %in% c("Octavos", "Cuartos", "Semis", "Final", "3_4_puesto"))
  
  partidos_fg = fase_grupos %>%select(SELECCION) %>% distinct()
  
  # Se crea el data frame
  
  Prom_seleccion_grupo = data.frame()
  
  # Se realiza el bucle
  
  for(i in 1:nrow(partidos_fg)){
    Seleccion=partidos_fg[["SELECCION"]][i]
    
    DATA= fase_grupos %>% filter(SELECCION==Seleccion)
    
    DATA= DATA %>% select_if(is.numeric)
    DATA = t(colMeans(DATA))
    
    Info=data.frame(SELECCION=Seleccion)
    
    DATA=cbind(DATA,Info)
    
    Prom_seleccion_grupo=rbind(Prom_seleccion_grupo, DATA)
  }
  
  # Se ha calculado para todas las variables pero el enunciado de este apartado solo pide la variable
  # HSR y la posesión, por lo que es lo que se va a mostrar en la siguiente tabla:
  
  kable(Prom_seleccion_grupo %>% select(SELECCION,HSR,posesion))
  
  # Ahora se va a representar mediante un diagrama de barras la HSR de las selecciones
  
  ggplot(Prom_seleccion_grupo, aes(SELECCION,HSR, fill=SELECCION))+
    geom_col(show.legend = FALSE)+
    coord_flip()+
    scale_y_continuous(expand = c(0,0))+
    theme(axis.title.y = element_blank())+
    ggtitle("HSR media en la fase de grupos")
  
}

# EJERCICO 13

{
  # Incluye una variable denominada “Clasifica” (de tipo factor) que tengo niveles: 
  # “Si”;”No”. En función de si la selección se clasificó para octavos o no
  
  # Compruebo que selecciones clasificaron a octavos
  
  octavos = Qatar1 %>% filter(Grupo %in% c("Octavos"))
  
  sel_octavos = octavos %>% select(SELECCION) %>% distinct()
  
  sel_octavos= sel_octavos$SELECCION
  
  # Ahora se crea una columna en Prom_seleccion_grupo que indica si esta clasificada o no
  
  Prom_seleccion_grupo = Prom_seleccion_grupo %>% mutate(
    Clasificada = ifelse(SELECCION %in% sel_octavos, "Si", "No")
  )
  
  kable(Prom_seleccion_grupo %>% select(SELECCION,HSR,posesion, Clasificada))
  
}

# EJERCICO 14

{
  # Analice si las selecciones clasificadas para octavos tuvieron un mayor rendimiento 
  # físico
  
  comparacion = Prom_seleccion_grupo %>%
    group_by(Clasificada) %>%
    summarize_if(is.numeric, list(mean = ~mean(.x, na.rm = TRUE)))
  
  comparacion$Clasificada = ifelse(comparacion$Clasificada == "Si", "Clasificadas", "Eliminadas")
  
  # PARÁMETROS DISTANCIA
  
  a1= ggplot(comparacion, aes(y=Clasificada, x=Total_Dist_mean, fill=Clasificada))+
    geom_col()+
    labs(x = "Distancia Total")+ 
    theme_minimal()+
    theme(axis.title.y = element_blank(),axis.text.y = element_blank(),legend.title= element_blank(),legend.position = "none")
  
  a2= ggplot(comparacion, aes(y=Clasificada, x=Dist_0.7_mean, fill=Clasificada))+
    geom_col()+
    labs(x = "Distancia 0-7 km/h")+ 
    theme_minimal()+
    theme(axis.title.y = element_blank(),axis.text.y = element_blank(),legend.title= element_blank(),legend.position = "none")
  
  a3= ggplot(comparacion, aes(y=Clasificada, x=Dist_7.15_mean, fill=Clasificada))+
    geom_col()+
    labs(x = "Distancia 7-15 km/h")+ 
    theme_minimal()+
    theme(axis.title.y = element_blank(),axis.text.y = element_blank(),legend.title= element_blank(),legend.position = "none")
  
  a4= ggplot(comparacion, aes(y=Clasificada, x=Dist_15.20_mean, fill=Clasificada))+
    geom_col()+
    labs(x = "Distancia 15-20 km/h")+
    theme_minimal()+
    theme(axis.title.y = element_blank(),axis.text.y = element_blank(),legend.title= element_blank(),legend.position = "none")
  
  a5= ggplot(comparacion, aes(y=Clasificada, x=Dist_20.25_mean, fill=Clasificada))+
    geom_col()+
    labs(x = "Distancia 20-25 km/h")+ 
    theme_minimal()+
    theme(axis.title.y = element_blank(),axis.text.y = element_blank(),legend.title= element_blank(),legend.position = "none")
  
  a6= ggplot(comparacion, aes(y=Clasificada, x=Dist_25_mean, fill=Clasificada))+
    geom_col()+
    labs(x = "Distancia +25 km/h")+ 
    theme_minimal()+
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(),legend.title= element_blank(),legend.position = "none")
  
  
  combinado_1 = ggarrange(a1,a2,a3,a4,a5,a6, nrow=2, ncol = 3, common.legend = TRUE, legend = "right")
  
  combinado_1 = annotate_figure(combinado_1, top = text_grob("Parámetros distancia", face = "bold", size = 14))
  
  combinado_1
  
  # OTROS PARAMETROS
  
  B1= ggplot(comparacion, aes(y=Clasificada, x=HSR_mean, fill=Clasificada))+
    geom_col()+
    labs(x = "HSR")+ 
    theme_minimal()+
    theme(axis.title.y = element_blank(),axis.text.y = element_blank(),legend.title= element_blank(),legend.position = "none")
  
  B2= ggplot(comparacion, aes(y=Clasificada, x=Sprints_n_mean, fill=Clasificada))+
    geom_col()+
    labs(x = "Sprints")+ 
    theme_minimal()+
    theme(axis.title.y = element_blank(),axis.text.y = element_blank(),legend.title= element_blank(),legend.position = "none")
  
  B3= ggplot(comparacion, aes(y=Clasificada, x=Vmax_mean, fill=Clasificada))+
    geom_col()+
    labs(x = "Vmax")+ 
    theme_minimal()+
    theme(axis.title.y = element_blank(),axis.text.y = element_blank(),legend.title= element_blank(),legend.position = "none")
  
  B4= ggplot(comparacion, aes(y=Clasificada, x=goles_favor_mean, fill=Clasificada))+
    geom_col()+
    labs(x = "Goles a favor")+
    theme_minimal()+
    theme(axis.title.y = element_blank(),axis.text.y = element_blank(),legend.title= element_blank(),legend.position = "none")
  
  B5= ggplot(comparacion, aes(y=Clasificada, x=TIEMPO_TOTAL_mean, fill=Clasificada))+
    geom_col()+
    labs(x = "Tiempo")+ 
    theme_minimal()+
    theme(axis.title.y = element_blank(),axis.text.y = element_blank(),legend.title= element_blank(),legend.position = "none")
  
  B6= ggplot(comparacion, aes(y=Clasificada, x=posesion_mean, fill=Clasificada))+
    geom_col()+
    labs(x = "Posesion")+ 
    theme_minimal()+
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(),legend.title= element_blank(),legend.position = "none")
  
  
  combinado_2 = ggarrange(B1,B2,B3,B4,B5,B6, nrow=2, ncol = 3, common.legend = TRUE, legend = "right")
  
  combinado_2 = annotate_figure(combinado_2, top = text_grob("Otros parámetros", face = "bold", size = 14))
  
  combinado_2

  }

# EJERCICO 15

{
  # Con el data frame “Prom_seleccion_grupo”, analice con un diagrama de dispersión si 
  # es cierto lo que comúnmente se dice: “Cuando no tienes balón corres más”.
  
  G1 = ggplot(Prom_seleccion_grupo, aes(y=posesion, x=Total_Dist))+
    geom_point()+
    labs(y = "Posesión", x="Distancia Total")+ 
    geom_smooth(method = "lm", se=FALSE)+
    theme_minimal()
    
  G2 = ggplot(Prom_seleccion_grupo, aes(y=posesion, x=Dist_0.7))+
    geom_point()+
    labs(y = "Posesión", x="Distancia 0-7 km/h")+ 
    geom_smooth(method = "lm", se=FALSE)+
    theme_minimal()
  
  G3 = ggplot(Prom_seleccion_grupo, aes(y=posesion, x=Dist_7.15))+
    geom_point()+
    labs(y = "Posesión", x="Distancia 7-15 km/h")+
    geom_smooth(method = "lm", se=FALSE)+
    theme_minimal()
  
  G4 = ggplot(Prom_seleccion_grupo, aes(y=posesion, x=Dist_15.20))+
    geom_point()+
    labs(y = "Posesión", x="Distancia 15-20 km/h")+
    geom_smooth(method = "lm", se=FALSE)+
    theme_minimal()
  
  G5 = ggplot(Prom_seleccion_grupo, aes(y=posesion, x=Dist_20.25))+
    geom_point()+
    labs(y = "Posesión", x="Distancia 20-25 km/h")+
    geom_smooth(method = "lm", se=FALSE)+
    theme_minimal()
  
  G6 = ggplot(Prom_seleccion_grupo, aes(y=posesion, x=Dist_25))+
    geom_point()+
    labs(y = "Posesión", x="Distancia +25 km/h")+
    geom_smooth(method = "lm", se=FALSE)+
    theme_minimal()
  
  G7 = ggplot(Prom_seleccion_grupo, aes(y=posesion, x=HSR))+
    geom_point()+
    labs(y = "Posesión", x="HSR")+
    geom_smooth(method = "lm", se=FALSE)+
    theme_minimal()

  
  dispersion_1 = ggarrange(G1,G2,G3, G4, ncol = 2, nrow = 2)
  
  dispersion_1 = annotate_figure(dispersion_1, top = text_grob("Gráficos de dispersión", face = "bold", size = 14))
  
  dispersion_1
  
  dispersion_2 = ggarrange(G5,G6,G7, ncol = 2, nrow = 2)
  
  dispersion_2 = annotate_figure(dispersion_2, top = text_grob("Gráficos de dispersión", face = "bold", size = 14))
  
  dispersion_2
  
}

# EJERCICO 16

{
  # Haga un análisis del mundial de España: incluya la información más relevante que 
  # considere tanto colectiva como individual o respecto a sus valores comparados con los 
  # de los rivales. Puede incluir gráficas
  
  # En primer lugar filtramos los datos por los partidos de España y se anañizan los top 5 jugadores españoles
  # de total_dist, HSR y vmax
  
  mundial_esp = Qatar1 %>% filter(grepl("ESP", PARTIDO, ignore.case=TRUE))
  Spain =  mundial_esp %>% filter(SELECCION=="España")
  
  top_5_dist_es= Spain %>% select(SELECCION,  PARTIDO, Name,Position, Total_Dist)
  
  kable(top_5_dist_es[order(-top_5_dist_es$Total_Dist),]%>% slice_head(n=5), 
        format = "html", 
        caption = "Top 5 jugadores con más distancia total en España") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
    column_spec(1, width = "3cm") %>%
    column_spec(2, width = "3cm") %>%
    column_spec(3, width = "3cm") %>%
    column_spec(4, width = "3cm") %>%
    column_spec(5, width = "3cm")
  
  
  kable(top_5_HSR_es[order(-top_5_HSR_es$HSR),]%>% slice_head(n=5), 
        format = "html", 
        caption = "Top 5 jugadores con más HSR en España") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
    column_spec(1, width = "3cm") %>%
    column_spec(2, width = "3cm") %>%
    column_spec(3, width = "3cm") %>%
    column_spec(4, width = "3cm") %>%
    column_spec(5, width = "3cm")
  
  
  top_5_Vmax_es= Spain %>% select(SELECCION,  PARTIDO,Name,Position, Vmax)
  
  kable(top_5_Vmax_es[order(-top_5_Vmax_es$Vmax),]%>% slice_head(n=5), 
        format = "html", 
        caption = "Top 5 jugadores con más Vmax en España") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
    column_spec(1, width = "3cm") %>%
    column_spec(2, width = "3cm") %>%
    column_spec(3, width = "3cm") %>%
    column_spec(4, width = "3cm") %>%
    column_spec(5, width = "3cm")
  
  # En segundo lugar se hace una media de las variables de España y su rival por cada uno de los partidos jugados
  
  rivales= mundial_esp %>% group_by(SELECCION, Jornada, PARTIDO) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE), .names = "mean_{col}"))
  
  
  # En tercer lugar se hace un análisis por jornada de cada uno de los partidos a ver como le fue a España
  
  # Parámetros de distancia
  
  M1= ggplot(rivales, aes(x=Jornada, y=mean_Total_Dist, fill=ifelse(SELECCION=="España", "España", "Otros")))+
    geom_col(position = "dodge")+
    labs(title = "Distancia Total",y = "Distancia Total")+# Agrega el titulo deseado
    theme_minimal()+
    guides(fill=guide_legend(title = NULL))
  
  M2= ggplot(rivales, aes(x=Jornada, y=mean_Dist_7.15, fill=ifelse(SELECCION=="España", "España", "Otros")))+
    geom_col(position = "dodge")+
    labs(title="Distancia 7-15 km/h", y = "Dist. 7-15 km/h")+# Agrega el titulo deseado
    theme_minimal()+
    guides(fill=guide_legend(title = NULL))
  
  M3= ggplot(rivales, aes(x=Jornada, y=mean_Dist_15.20, fill=ifelse(SELECCION=="España", "España", "Otros")))+
    geom_col(position = "dodge")+
    labs(title="Distancia 15-20 km/h", y="Dist. 15-20 km/h")+# Agrega el titulo deseado
    theme_minimal()+
    guides(fill=guide_legend(title = NULL))
  
  M4= ggplot(rivales, aes(x=Jornada, y=mean_HSR, fill=ifelse(SELECCION=="España", "España", "Otros")))+
    geom_col(position = "dodge")+
    labs(title="HSR", y="HSR")+# Agrega el titulo deseado
    theme_minimal()+
    guides(fill=guide_legend(title = NULL))
  
  analisis_mundial_1 = ggarrange(M1,M2,M3, M4, ncol = 2, nrow = 2,common.legend = TRUE, legend = "right")
  
  analisis_mundial_1 = annotate_figure(analisis_mundial_1, top = text_grob("Parámetros de distancia", face = "bold", size = 14))
  
  analisis_mundial_1
  
  # Otros parámetros
  
  M5= ggplot(rivales, aes(x=Jornada, y=mean_Sprints_n, fill=ifelse(SELECCION=="España", "España", "Otros")))+
    geom_col(position = "dodge")+
    labs(title = "Sprints",y = "Nº Sprints")+# Agrega el titulo deseado
    theme_minimal()+
    guides(fill=guide_legend(title = NULL))
  
  M6= ggplot(rivales, aes(x=Jornada, y=mean_Vmax, fill=ifelse(SELECCION=="España", "España", "Otros")))+
    geom_col(position = "dodge")+
    labs(title="Vmax", y = "Vmax")+# Agrega el titulo deseado
    theme_minimal()+
    guides(fill=guide_legend(title = NULL))
  
  M7= ggplot(rivales, aes(x=Jornada, y=mean_goles_favor, fill=ifelse(SELECCION=="España", "España", "Otros")))+
    geom_col(position = "dodge")+
    labs(title="Goles a favor", y="Goles favor")+# Agrega el titulo deseado
    theme_minimal()+
    guides(fill=guide_legend(title = NULL))
  
  M8= ggplot(rivales, aes(x=Jornada, y=mean_posesion, fill=ifelse(SELECCION=="España", "España", "Otros")))+
    geom_col(position = "dodge")+
    labs(title="Posesion", y="Posesion")+# Agrega el titulo deseado
    theme_minimal()+
    guides(fill=guide_legend(title = NULL))
  
  analisis_mundial_2 = ggarrange(M5,M6,M7, M8, ncol = 2, nrow = 2,common.legend = TRUE, legend = "right")
  
  analisis_mundial_2 = annotate_figure(analisis_mundial_2, top = text_grob("Otros parámetros", face = "bold", size = 14))
  
  analisis_mundial_2
  
  # Ahora se analizan los jugadores por jornada, se analiza vmzx, total_dist y HSR

  
 jornada_1 = mundial_esp %>% filter(Jornada == "1")
 jornada_2 = mundial_esp %>% filter(Jornada == "2")
 jornada_3 = mundial_esp %>% filter(Jornada == "3")
 jornada_4 = mundial_esp %>% filter(Jornada == "Octavos")
 
 # JORNADA 1
 
 top_5_dist_1= jornada_1 %>% select(SELECCION, Name, Surname,Position, Total_Dist)
 
 kable(top_5_dist_1[order(-top_5_dist_1$Total_Dist),]%>% slice_head(n=5), 
       format = "html", 
       caption = "Top 5 jugadores con más distancia total en la jornada 1") %>%
   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
   column_spec(1, width = "3cm") %>%
   column_spec(2, width = "3cm") %>%
   column_spec(3, width = "3cm") %>%
   column_spec(4, width = "3cm") %>%
   column_spec(5, width = "3cm")
 
 top_5_HSR_1= jornada_1 %>% select(SELECCION, Name, Surname,Position, HSR)
 
 kable(top_5_HSR_1[order(-top_5_HSR_1$HSR),]%>% slice_head(n=5), 
       format = "html", 
       caption = "Top 5 jugadores con más HSR en la jornada 1") %>%
   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
   column_spec(1, width = "3cm") %>%
   column_spec(2, width = "3cm") %>%
   column_spec(3, width = "3cm") %>%
   column_spec(4, width = "3cm") %>%
   column_spec(5, width = "3cm")
 
 
 top_5_Vmax_1= jornada_1 %>% select(SELECCION, Name, Surname,Position, Vmax)
 
 kable(top_5_Vmax_1[order(-top_5_Vmax_1$Vmax),]%>% slice_head(n=5), 
       format = "html", 
       caption = "Top 5 jugadores con más Vmax en la jornada 1") %>%
   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
   column_spec(1, width = "3cm") %>%
   column_spec(2, width = "3cm") %>%
   column_spec(3, width = "3cm") %>%
   column_spec(4, width = "3cm") %>%
   column_spec(5, width = "3cm")
 
 
 # JORNADA 2
 
 top_5_dist_2= jornada_2 %>% select(SELECCION, Name, Surname, Position, Total_Dist)
 
 kable(top_5_dist_2[order(-top_5_dist_2$Total_Dist),]%>% slice_head(n=5), 
       format = "html", 
       caption = "Top 5 jugadores con más distancia total en la jornada 2") %>%
   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
   column_spec(1, width = "3cm") %>%
   column_spec(2, width = "3cm") %>%
   column_spec(3, width = "3cm") %>%
   column_spec(4, width = "3cm") %>%
   column_spec(4, width = "3cm") %>%
   column_spec(5, width = "3cm")

 
 top_5_HSR_2= jornada_2 %>% select(SELECCION, Name, Surname,Position, HSR)
 
 kable(top_5_HSR_2[order(-top_5_HSR_2$HSR),]%>% slice_head(n=5), 
       format = "html", 
       caption = "Top 5 jugadores con más HSR en la jornada 2") %>%
   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
   column_spec(1, width = "3cm") %>%
   column_spec(2, width = "3cm") %>%
   column_spec(3, width = "3cm") %>%
   column_spec(4, width = "3cm") %>%
   column_spec(5, width = "3cm")
 
 top_5_Vmax_2= jornada_2 %>% select(SELECCION, Name, Surname,Position, Vmax)
 
 kable(top_5_Vmax_2[order(-top_5_Vmax_2$Vmax),]%>% slice_head(n=5), 
       format = "html", 
       caption = "Top 5 jugadores con más Vmax en la jornada 2") %>%
   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
   column_spec(1, width = "3cm") %>%
   column_spec(2, width = "3cm") %>%
   column_spec(3, width = "3cm") %>%
   column_spec(4, width = "3cm") %>%
   column_spec(5, width = "3cm")


# JORNADA 3

 
 top_5_dist_3= jornada_3 %>% select(SELECCION, Name, Surname,Position, Total_Dist)
 
 kable(top_5_dist_3[order(-top_5_dist_3$Total_Dist),]%>% slice_head(n=5), 
       format = "html", 
       caption = "Top 5 jugadores con más distancia total en la jornada 3") %>%
   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
   column_spec(1, width = "3cm") %>%
   column_spec(2, width = "3cm") %>%
   column_spec(3, width = "3cm") %>%
   column_spec(4, width = "3cm") %>%
   column_spec(5, width = "3cm")

 
 top_5_HSR_3 = jornada_3%>% select(SELECCION, Name, Surname,Position, HSR)
 
 kable(top_5_HSR_3[order(-top_5_HSR_3$HSR),]%>% slice_head(n=5), 
       format = "html", 
       caption = "Top 5 jugadores con más HSR en la jornada 3") %>%
   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
   column_spec(1, width = "3cm") %>%
   column_spec(2, width = "3cm") %>%
   column_spec(3, width = "3cm") %>%
   column_spec(4, width = "3cm") %>%
   column_spec(5, width = "3cm")
 
 
 top_5_Vmax_3= jornada_3 %>% select(SELECCION, Name, Surname,Position, Vmax)
 
 kable(top_5_Vmax_3[order(-top_5_Vmax_3$Vmax),]%>% slice_head(n=5), 
       format = "html", 
       caption = "Top 5 jugadores con más Vmax en la jornada 3") %>%
   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
   column_spec(1, width = "3cm") %>%
   column_spec(2, width = "3cm") %>%
   column_spec(3, width = "3cm") %>%
   column_spec(4, width = "3cm") %>%
   column_spec(5, width = "3cm")
 
 # JORNADA 4
 
 top_5_dist_4= jornada_4 %>% select(SELECCION, Name, Surname,Position, Total_Dist)
 
 kable(top_5_dist_4[order(-top_5_dist_4$Total_Dist),]%>% slice_head(n=5), 
       format = "html", 
       caption = "Top 5 jugadores con más distancia total en octavos") %>%
   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
   column_spec(1, width = "3cm") %>%
   column_spec(2, width = "3cm") %>%
   column_spec(3, width = "3cm") %>%
   column_spec(4, width = "3cm") %>%
   column_spec(5, width = "3cm")
 
 
 top_5_HSR_4= jornada_4 %>% select(SELECCION, Name, Surname,Position, HSR)
 
 kable(top_5_HSR_4[order(-top_5_HSR_4$HSR),]%>% slice_head(n=5), 
       format = "html", 
       caption = "Top 5 jugadores con más HSR en octavos") %>%
   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
   column_spec(1, width = "3cm") %>%
   column_spec(2, width = "3cm") %>%
   column_spec(3, width = "3cm") %>%
   column_spec(4, width = "3cm") %>%
   column_spec(5, width = "3cm")

 
 top_5_Vmax_4= jornada_4 %>% select(SELECCION, Name, Surname,Position, Vmax)
 
 kable(top_5_Vmax_4[order(-top_5_Vmax_4$Vmax),]%>% slice_head(n=5), 
       format = "html", 
       caption = "Top 5 jugadores con más Vmax en octavos") %>%
   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
   column_spec(1, width = "3cm") %>%
   column_spec(2, width = "3cm") %>%
   column_spec(3, width = "3cm") %>%
   column_spec(4, width = "3cm") %>%
   column_spec(5, width = "3cm")
}
