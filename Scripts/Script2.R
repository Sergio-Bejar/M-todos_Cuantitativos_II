### Cargar paquetes

library(tidyverse)
library(stevemisc)

## Calcular diferencia en tamaña de EMA. Asumimos componente poblacional igual a 28

28/sqrt(100)
28/sqrt(400)

## Calcular cambios en tamaño de EMA al incrementar muestra de (a) 100 a 1000 y (b) 1000 a 10,000

(28/sqrt(100))/(28/sqrt(1000))
(28/sqrt(1000))/(28/sqrt(10000))

#########################################
## Plot dist. normal con varianza alta ##
#########################################

n = 20000

## creamos tabla con dos columnas:  uid = secuencia de 1 a 20,000,  therm = valor aleatorio de un dist. normal (bounded) con media = 58 y d.e. = 15
## esta tabla se llama "highvar"

tibble(uid = seq(1:n),
       therm = rbnorm(n, 58, 15, 0, 100, round=TRUE, seed=8675309)) -> highvar

## generamos el gráfico para "highvar"

ggplot(highvar, aes(therm)) + 
  theme_minimal() +
  theme(plot.title = element_text(size=20)) +
  geom_histogram(binwidth = 1, alpha=0.8, color="black") +
  scale_x_continuous(breaks = seq(0, 100, by =10), limits=c(0,100))  +
  labs(title = "Desviación Estándar Alta",
       subtitle = "Datos simulados para tener una frontera de distribución normal (distribución beta) con media de 58 y desviación estándar de 15.",
       y = "Frecuencia", x = "Polarización",
       caption = "Datos hipotéticos.")

#########################################
## Plot dist. normal con varianza baja ##
#########################################

n = 20000

## la diferencia aquí es que la dist. normal es calculada con una d.e. = 6. Para entender lo demás, ver arriba. 

tibble(uid = seq(1:n),
       therm = rbnorm(n, 58, 6, 0, 100, round=TRUE, seed=8675309)) -> lowvar

ggplot(lowvar, aes(therm)) + 
  theme_minimal() +
  theme(plot.title = element_text(size=20)) +
  geom_histogram(binwidth = 1, alpha=0.8, color="black") +
  scale_x_continuous(breaks = seq(0, 100, by =10), limits=c(0,100))  +
  labs(title = "Desviación Estándar Baja",
       subtitle = "Datos simulados para tener una frontera de distribución normal (distribución beta) con media de 34 y desviación estándar de 6.",
       y = "Frecuencia", x = "Polarización",
       caption = "Datos hipotéticos.")

######################################################################
## plots de medias muestrales para diferentes n's con alta varianza ##
#####################################################################

## crea vector con valores de diferentes tamaños de muestra

sample_sizes <- c(10, 25, 100, 400, 1000)

## definimos Samps que es una lista 

Samps = list() 

set.seed(8675309)

## for loop para generar 10 muestras aleatorias diferentes para cada uno de los valores de sample_sizes. Luego calcula el valor de la media para cada muestra.

for (j in sample_sizes) {
  Samps[[paste0("Sample size: ", j)]] = data.frame(sampsize=j, samp=sapply(1:10, function(i){ x <- sample(highvar$therm, j, replace = TRUE) }))
}

## creamos data.frame

Samps %>%
  map_df(as_tibble) %>%
  gather(samp, value, samp.1:samp.10) -> Samps

# Plot de la medias muestrales para cada tamaño de la muestra

Samps %>%
  group_by(sampsize, samp) %>%
  summarize(sampmean = mean(value)) %>%
  ggplot(., aes(as.factor(sampsize),sampmean)) + 
  geom_point(size=3, color="black", alpha=0.5) +
  theme_minimal() + 
  theme(plot.title = element_text(size=20)) +
  geom_hline(yintercept = mean(highvar$therm), linetype="dashed") +
  labs(x = "Tamaño de la Muestra",
       y = "Medias Muestrales",
       title = "Díez Medias Muestrales Obtenidas Variando el Tamaño de la Muestra en una Población con Alta Varianza",
       subtitle = "Los rendimientos decrecientes de incrementar el tamaño de la muestra comienzan al rededor de las 400 observaciones aunque el spread en estos datos simulados es bastante grande.",
       caption = "Datos hipotéticos generados con los siguientes parámetros: media= 58, d.e.= 15, n=20,000")


######################################################################
## plots de medias muestrales para diferentes n's con baja varianza ##
#####################################################################

Samps = list() 
set.seed(8675309)
for (j in sample_sizes) {
  Samps[[paste0("Sample size: ", j)]] = data.frame(sampsize=j, samp=sapply(1:10, function(i){ x <- sample(lowvar$therm, j, replace = TRUE) }))
}

Samps %>%
  map_df(as_tibble) %>%
  gather(samp, value, samp.1:samp.10) -> Samps

Samps %>%
  group_by(sampsize, samp) %>%
  summarize(sampmean = mean(value)) %>%
  ggplot(., aes(as.factor(sampsize),sampmean)) + 
  geom_point(size=3, color="black", alpha=0.5) +
  theme_minimal() + 
  theme(plot.title = element_text(size=20)) +
  geom_hline(yintercept = mean(lowvar$therm), linetype="dashed") +
  labs(x = "Tamaño de la Muestra",
       y = "Medias Muestrales",
       title = "Díez Medias Muestrales Obtenidas Variando el Tamaño de la Muestra en una Población con Alta Varianza",
       subtitle = "Los rendimientos decrecientes de incrementar el tamaño de la muestra comienzan al rededor de las 400 observaciones aunque el spread en estos datos simulados es bastante grande.",
       caption = "Datos hipotéticos generados con los siguientes parámetros: media= 58, d.e.= 15, n=20,000")
