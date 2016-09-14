# Cargo el dataset
glx <- read.csv("http://astrostatistics.psu.edu/datasets/COMBO17.csv", header = T, stringsAsFactors = F)

# Miro los datos
str(glx)

# Como encontre que hay una var q tomo como caracter, la fuerzo a numeric
v_error1 <- as.numeric(glx$e.W420FE)

# Miro cuantos no pudo convertir
table(is.na(v_error1))

# Me fijo cuales son los datos del dataset original que no pudo convertir
glx$e.W420FE[is.na(v_error1)]

# Pregunto cuales son los indices del dataset
v_error1_idx <- which(is.na(v_error1))

# Trato de corregir el error de los datos (NO HAGAN ESTO EN SUS CASAS, solo para practicar)
# Spliteo el campo, que tiene dos espacios en el medio
v_error_lista <- strsplit(glx$e.W420FE[v_error1_idx],"  ")
v_error_lista[[143]][2]
# Recorro la lista y en cada vector selecciono la posicion 2, que es donde está el valor que quiero obtener
# en el mismo paso lo paso a numeric
v_error_fix <- as.numeric(sapply(v_error_lista,"[[",2))

# Asigno los valores corregidos al dataset
glx$e.W420FE[v_error1_idx] <- v_error_fix
# Verifico que no haya error
table(is.na(as.numeric(glx$e.W420FE)))
# Convierto a numeric la columna
glx$e.W420FE <- as.numeric(glx$e.W420FE)
# VOILA!!!
str(glx)

install.packages("ggplot2")
# La gramatica de los graficos
library(ggplot2)

# Histogramas
hist(glx$ApDRmag)

# Diagrama de Densidad
plot(density(glx$ApDRmag))

# Boxplot
boxplot(glx$ApDRmag)

# Graficos con QPLOT
qplot(ApDRmag, data=glx)

# hago un boxplot con qplot
qplot(factor(0), ApDRmag, geom = "boxplot", xlab="", data=glx)

# grafico de dispersion
qplot(ApDRmag, Rmag, data=glx)

# histograma con ggplot()
ggplot(glx, aes(x = ApDRmag)) + geom_histogram()

# El gráfico de densidad kernel
ggplot(glx, aes(x = ApDRmag)) + geom_density(kernel = "epanechnikov")

# Ambos combinados
  ggplot(glx, aes( x = ApDRmag) ) +  
  geom_histogram( aes(y = ..density..), color = "black", fill = "light blue") +
  geom_density( kernel = "epanechnikov", color="red")

# Para datos faltantes
  apply(glx,2,anyNA)
  # aplico al dataframe, recorriendo por columna(2), la función anyNA para saber si hay
  # valores NA