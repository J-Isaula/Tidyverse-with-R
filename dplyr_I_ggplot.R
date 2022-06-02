#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MANIPULACIÓN DE BASES DE DATOS USANDO dplyr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Cargamos las librerías 
library(tidyverse)
library(dplyr)
library(tinytex)
library(lubridate)
library(ggplot2)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
misdatos <- read_csv(file="DataDplyr.csv")
view(misdatos)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                          Seleccionamos variables                         %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

misdatos %>%
  select(id,nombre,edad,sexo) %>% 
  head(2)

misdatos %>% 
  select(1,2,3,4) %>% 
  head(2)

misdatos %>% 
  select(-nombre,-edad) %>% 
  head(n=3)

misdatos %>% 
  select(starts_with("Zona")) %>%  # Me selecciona solo las variables que comienzan con "Zona"
  head(n=3)

misdatos %>%  
  select(nombre:sexo) %>%         # Si queremos un bloque de variables
  head(n=3)


misdatos %>% 
  select(!(nombre:sexo)) %>%      #  me trae toda la base de datos menos las variables que esten contenidas en ese rango
  head(n=3)

misdatos %>%  
  select(matches("e+so")) %>%  head(2)  # seleciona variables que tienen una "e" o "so" dentro.

misdatos %>% 
  select_if(is.numeric) %>% 
  head(2)

misdatos %>%
  select_if(~!is.numeric(.)) %>%    # ~ este signo se usa cuando estamos colocando una funcion que es propia.
  head(n=3)

misdatos %>%
  select_if(is.numeric) %>% 
  select_if(~sd(.,na.rm = TRUE)>10) %>%     
  head(3)

misdatos %>% 
  select_if(is.numeric) %>% 
  select_if(~mean(., na.rm = TRUE)>2) %>% 
  head(n=3)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                  FILTROS                                  %
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

misdatos %>% 
  filter(edad>=18 & !zona_prov=="Ocoa") %>% 
  select(nombre,edad) %>% 
  head()

misdatos %>%
  filter(zona_prov=="Ocoa") %>%  # Filtro de variables en cadena 
  head(3)

misdatos %>% 
  filter(peso>= mean(peso,na.rm = TRUE)) %>% 
  head(3)

filter(misdatos,peso>=mean(peso,na.rm = TRUE))[1:3,]

misdatos %>% 
  select(everything()) %>% 
  filter(ingresos > quantile(ingresos,0.25),zona_prov=="Ocoa")       # vamos a obtener una probabilidad de 25% 



misdatos %>% 
  select(everything()) %>% 
  filter(ingresos > quantile(ingresos,0.25),zona_prov %in% c("Ocoa","Bani"))    # %in% uso de esta función 



misdatos %>% 
  select(everything()) %>% 
  filter(ingresos>5000, ingresos<8000)                  # Una forma de obtener un rango 5000 < ingresos < 8000


misdatos %>% 
  select(everything()) %>% 
  filter(between(ingresos,5000,8000))                   # Mismos resultados que el anterior pero con funcion between

misdatos %>% 
  select(everything()) %>% 
  filter(between(ingresos,5000,8000))

misdatos %>% 
  select(everything()) %>% 
  filter(ingresos>(5000-100) & ingresos<(5000+100))

misdatos %>% 
  select(everything()) %>% 
  filter(near(ingresos, 5000,100))                                              # Intervalo, usando "near"


misdatos %>% 
  select(everything()) %>% 
  filter(near(ingresos,mean(ingresos),sd(ingresos)))   

misdatos %>% 
  select(everything()) %>% 
  filter(near(ingresos,mean(ingresos),3*sd(ingresos)))                             # Datos atipicos, recordar que son los que estan a 3 desviaciones estandar

misdatos %>% 
  select(everything()) %>% 
  filter(near(ingresos,mean(ingresos),3*sd(ingresos)),!ingresos==0)                # No imprimir las personas que son de ingreso cero



misdatos %>% 
  filter(is.na(peso)) %>%                                                          # is.na identifica valores de tipo NA
  head(3)


misdatos %>% 
  select(nombre,ingresos,zona_prov) %>%                                            # Escribimos cantidad de filas usando Slice
  slice(1:3)

misdatos %>% 
  slice(which.min(edad))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                     CREAR VARIABLES (MUTATE)                                         %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Mutate: genera variables
# 1. Agrega nuevas columnas con "mutate".


misdatos %>% 
  select(everything()) %>% 
  filter(!is.na(peso)) %>% 
  mutate(logedad = log(edad),
         qedad= edad^2,
         mayores = (edad>=18)*1,
         mayoresEdad = (edad>=18)*edad,
         zedad=scale(edad),
         porcentajeing = (ingresos/sum(ingresos))*100) %>% 
  head()

# 2. "case_when": para tener varias condiciones a la vez 

misdatos %>% 
  select(id,nombre,edad,sexo) %>% 
  mutate(PET=case_when(
    edad<=17 ~ "menor",
    edad>=18 & edad <64 ~ "PET",
    edad>=64 ~ "Mayor")) %>% 
  mutate(PET = factor(PET)) %>% 
  head()

# 3. "recode": permite recodificar variables, permite intercambiar los valores observados. 

misdatos %>% 
  select(id,nombre, zona_res) %>% 
  mutate(Rzona_res = recode(zona_res,
                            "Urbana"="urbana",
                            "Rural" = "rural")) %>% 
  head()

# 4. Mutate_if: se utiliza para resumir tareas

misdatos %>% 
  select(-id) %>% 
  mutate_if(is.numeric,log) %>% 
  head()

# 5. Mutate_at:  permite convertir un rango de variables determinadas a las cuales transforma

misdatos %>% 
  select(-id) %>% 
  mutate_at(vars(peso,ingresos),~(.^2)) %>% 
  rename_at(vars(peso,ingresos),~paste0("qua_",.)) %>% 
  head()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                         Análisis por grupo                                           %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 1. El summarize permite  realizar un "collapse" de una base de datos, es decir agregar bases de datos. 

misdatos %>% 
  group_by(zona_prov) %>% 
  summarize(promedio = mean(ingresos,na.rm=TRUE),
            promedioe=mean(edad,na.rm=TRUE))
# Summarise: cut

# 1. Ingreso promedio por rangos de edad:

misdatos3 <- misdatos %>% 
  select(id,zona_prov,edad,ingresos) %>% 
  mutate(Ran.edad=cut(edad, breaks = seq(0,100,10)))

misdatos3 %>% 
  group_by(Ran.edad) %>% 
  summarise(mediaEdad=mean(ingresos))
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                         ggplot2: Motivate                                            %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
ggplot(data=misdatos3, 
       mapping = aes(x = Ran.edad, fill = Ran.edad)) +
  geom_bar() + 
  scale_fill_brewer(type="qual") + 
  theme_minimal()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                         Resumen Estadistico                                          %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

misdatos %>% 
  group_by(zona_prov) %>% 
  summarise(no_obs=n(),
            ing.medio = mean(ingresos),
            ing_min = min(ingresos),
            ing_max = max(ingresos),
            Rangor = ing_max - ing_min,
            p1 = first(ingresos),
            p2 = last(ingresos),
            q1 = quantile(ingresos,0.25))

# 1. Conjunto estadisticos para una o un grupo de variables

misdatos %>% 
  group_by(zona_res) %>% 
  summarise_at(vars(edad),
               list(~median(.), ~sd(.), ~IQR(.)))

# 2. Varias variables de agrupamiento

misdatos %>% 
  group_by(zona_prov,zona_res) %>% 
  summarise(mean(ingresos))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                            ggplot                                                    %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 1. Ejemplo Preliminar

theme_set(theme_minimal())

ggplot(iris,aes(y=Sepal.Length,x=Petal.Length)) + 
  geom_point()+
  theme_minimal()

# 2. Modificar cualidades de los gráficos


ggplot(iris,aes(y=Sepal.Length,x=Petal.Length)) + 
  geom_point()+
  xlab("Label x") + ylab("Label y") + ggtitle("Seminario")+
  theme_minimal()
         
# 3. Podemos condicionar el color de los puntos alguna variable.

iris %>% 
  ggplot(aes(y=Sepal.Length,x=Petal.Length,colour=Species)) +
  geom_point()+
  theme(legend.position = "top")

# 4. Podemos condicionar el simbolo de los puntos usando shape:

iris %>%
  ggplot(aes(y=Sepal.Length,x=Petal.Length, colour=Species,shape=Species)) +
  geom_point()+
  theme(legend.position = "top")
  
# 5. Cambiar escala de colores

iris %>% 
  ggplot(aes(y=Sepal.Length, x=Petal.Length)) +
  geom_point(aes(shape=Species,color=Species))+
  scale_fill_viridis_d()
  
# 6. Plantillas de temas integrados en ggplot (theme_*) (classic,bw),void) o en otros  
#    paquetes como ggtheme(fivethirtyeight, tufte,wsj).
#    Set your theme as the default using theme_set().

iris %>% 
  ggplot(aes(y=Sepal.Length,x=Petal.Length,color=Species))+
  geom_point()+
  theme_classic()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                            Estructura de la Data                                     %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Obtener un resumen sobre la estructura de la data.
library(wooldridge)
library(gapminder)

apple %>% 
  glimpse()

# Gráfico de barras: Frecuencia

apple %>% 
  filter(state %in% c("SD","KS","MI","TN","NY")) %>% 
  ggplot()+
  geom_bar(mapping = aes(x=state))

# Gráfico de barras horizontales: Frecuencia

apple %>% 
  filter(state %in% c("SD","KS","MI","TN","NY")) %>% 
  ggplot()+
  geom_bar(mapping = aes(x=state))+
  coord_flip()

# Gráfico de barras: Frecuencia

library(forcats)

apple %>% 
  filter(state %in% c("SD","KS","MI","TN","NY")) %>% 
  ggplot()+
  geom_bar(mapping = aes(x=forcats::fct_infreq(state)))

# Gráfico de barras: resumen

apple %>% 
  filter(state %in% c("SD","KS","MI","TN","NY")) %>% 
  group_by(state) %>% 
  summarise(mediaE=mean(educ)) %>% 
  ggplot(aes(x=state,y=mediaE))+
  geom_bar(stat = "identity",fill="steelblue")+
  geom_text(aes(label=round(mediaE,2)),vjust=1.6,color="white")+
  theme_minimal()

# Gráfico en términos de dispersión.

apple %>% 
  filter(state %in% c("SD","KS","MI","TN","NY")) %>% 
  group_by(state) %>% 
  summarise(mediaE=mean(educ)) %>% 
  mutate(zmedia=scale(mediaE)) %>% 
  ggplot(aes(x=state,y=zmedia,label=zmedia))+
  geom_bar(stat="identity",fill="steelblue")+
  coord_flip()


apple %>%
  filter(state %in% c("SD","KS","MI","TN","NY"))%>%
  group_by(state)%>%
  summarise(mediaE =mean(educ))%>%
  mutate(zmedia =scale(mediaE),meditipo =ifelse(zmedia<0,"Debajo","Encima"))%>%
  arrange(mediaE) ->data

ggplot(data5,
  aes(x =state,y=zmedia,label=zmedia))+
  geom_bar(stat="identity",aes(fill=meditipo))+
  coord_flip() ->g4;g4

g4 +
  scale_fill_manual(name="Ingreso medio",
                    label=c("Debajo","Encima"),
                    values = c("Debajo"="#00ba38","Encima"="#f8766d"))

# Gráfico de barras: resumen de dispersión

ggplot(data5,aes(x=state,y=round(zmedia,1),label=round(zmedia,1)))+
  geom_point(stat="identity",fill="black",size=6)+
  geom_segment(aes(y=0,
                   x=state,
                   yend=round(zmedia,1),
                   xend=state),
               color="black")+
  geom_text(color="white",size=2)+coord_flip()
  

# Gráfico de barras II

apple %>% 
  filter(state %in% c("SD","KS","MI","TN","NY")) %>% 
  group_by(state) %>% 
  summarise(mediaE=mean(educ)) %>% 
  ggplot(aes(x=state,y=mediaE,fill=state))+
  geom_bar(stat="identity")+xlab("Estados")+ylab("Años de escolaridad")+
  geom_text(aes(label=round(mediaE,2)),vjust=1.6,color="white")+
  scale_fill_manual(name="Estados",values=c("steelblue1","hotpink4",
                                            "limegreen","chocolate4",
                                            "darkviolet"))
  
  apple %>% 
    filter(state %in% c("SD","KS","MI","TN","NY")) %>% 
    group_by(state,male) %>% 
    summarise(ingreso=sum(faminc)) %>% 
    ggplot(aes(x=state,y=ingreso,fill=factor(male)))+
    geom_bar(stat="identity")+
    labs(title = "Gráfico del ingreso familiar")
  
  
  # Gráfico para comparar promedios entre grupos
  
  apple %>% 
    filter(state %in% c("SD","KS","MI","TN","NY")) %>% 
    group_by(state,male) %>% 
    summarise(ingreso=mean(faminc)) %>% 
    ggplot(aes(x=state,y=ingreso,fill=factor(male)))+
    geom_bar(stat="identity",position = position_dodge(width = 0.2))
    
  
  # Gráfico de pastel
  
  apple %>% 
    filter(state %in% c("SD","KS","MI","TN","NY")) %>% 
    group_by(state) %>% 
    summarise(obs=n()) %>% 
    ggplot(aes(x="",y=obs,fill=state))+
    geom_bar(width=1,stat = "identity")+
    coord_polar("y",start = 0)+
    theme_minimal()
  
  
  
  
  
  