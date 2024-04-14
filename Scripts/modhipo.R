# En este ejemplo vamos a ver la relación que existe entre el desarrollo económico y nivel de democracia en Latinoamérica.
# Esto es, analizaremos la validez de la hipótesis de modernización y para ello primero vamos a generar una 
# base que usa datos de las siguientes fuentes: (i) World Development Indicators (Banco Mundial, WDI) y (ii) Polity Project.

# Llamamos las librerias que vamos a necesitar (si no las tienes instaladas, hay que instalarlas primero)

library(WDI) # para bajar datos de World Development Indicators
library(countrycode) # para hacer merge entre datos de WDI y Polity
library(dplyr) # para manipular datos
library (lmtest) # para hacer diagnósticos de nuestra regresión
library(ggplot2) # para graficar
library(stargazer) # para hacer tablas bonitas
library(foreign) # para leer un archivo en SPSS con los datos de Polity
library(scales) # para re-escalar variables

#########################################
#         Creamos Datos                 #
#########################################

# Ahora creamos la base con la que trabajaremos
# Bajamos datos de desarrollo económico de la API del Banco Mundial
# Nota: para este ejercicio sólo bajaremos datos de PIB per capita y crecomiento del PIB

wdi <- WDI(country=c('AR', 'BO', 'BR', 'CO', 'CL', 'CR', 'EC', 'GT', 'HN', 'MX', 'NI', 'PA', 'PE', 'PY', 'SV', 'UY','VE'), 
           indicator=c('NY.GDP.MKTP.KD.ZG', 'NY.GDP.PCAP.KD'), 
           start = 1970, end = 2015, 
           extra = TRUE)
wdi$cowcode <- countrycode(wdi$iso2c, "iso2c", "cown") # para hacer compatible con datos de POLITY (códigos de país)
wdi <- rename(wdi, gdppc = NY.GDP.PCAP.KD, gdpgrowth = NY.GDP.MKTP.KD.ZG) # renombramos variable de GDPPC

polity <- read.spss("http://www.systemicpeace.org/inscr/p4v2016.sav", to.data.frame = TRUE) # para bajar los datos de POLITY

polity <- subset(polity, year >=1970 & year<=2015) # limitamos periodo de 1970 a 2015
variables <- c("ccode", "scode", "country", "year", "polity2") # seleccionamos las variables que necesitamos
polity <-polity[variables]  # creamos data.frame únicamente con variables que necestiamos
polity <- rename(polity, cowcode = ccode) # renombramos variable para poder hacer merge con wdi
polity$polity2.r <- rescale(polity$polity2) # reescalamos el índice de polity de rango (-10 a 10) a rabgo (0 a 1) 

############################### 
# Hacemos merge de ambas bases #
################################

wdi_polity <- merge(wdi, polity, by = c("cowcode", "year")) # merge en cowcode y year
wdi_polity <- select(wdi_polity, gdpgrowth, gdppc, polity2, polity2.r, year, country.x, cowcode) # seleccionamos variables que usaremos
wdi_polity$loggdppc <- log(wdi_polity$gdppc) # logaritmo de GDPPC

# Ya tenemos la base que necesitamos para probar la reación descrita al iniciar

##################################
#       Análisis Preliminar      #
##################################


ggplot(wdi_polity, aes(x=loggdppc, y=polity2.r)) + 
  geom_point( color="#69b3a2")                         # scatter plot básico

ggplot(wdi_polity, aes(x=loggdppc, y=polity2.r)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=TRUE)  # scatetter plot con tendencia lineal e I.C.

#######################################
#     Regresión Lineal Simple         #
#######################################
 
modelo1 <-lm(polity2.r ~ loggdppc, data = wdi_polity) # estimamos modelo de OLS 
summary(modelo1) # llamamos al objeto que contiene los resultados

stargazer(modelo1, type = "html")
