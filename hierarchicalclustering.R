#Clustering jerarquico

#Importamos nuestra base de datos de la encuesta y la etiquetamos como "myData" 
setwd("C:/Users/luzy3/OneDrive/IMÁGENES/IMAGENES/Documents/INVESTIGACIÓN/ENCUENTRO GARZA/Hierarchical clustering")

myData <- read.csv(file="data.csv", header= TRUE)
Surveyed <- myData$ENCUESTADO

#Instalamos y cargamos la libreria "cluster", "ggplot2", "factoextra" y "openxlsx".
library (cluster)
library(ggplot2)
library (factoextra)
library(openxlsx)

#La primera variable del conjunto de datos contiene los numeros de identificacion de los encuestados y no se utilizara en el analisis de agrupaciones. 
#Usamos la funcion de "daisy" para determinar la similitud entre las observaciones y etiquetar los resultados como d. 
#Para las opciones dentro de la funcion "daisy", usamos metrica para especificar el calculo de la distancia. 
#Las opciones para el calculo de la distancia incluyen "euclidiana", "manhattan" y "gower". Especificamos "gower"
d <- daisy(myData[,2:29], metric="gower")
d

#Usamos la funcion agnes con el metodo de Ward para realizar agrupaciones aglomerativas y etiquetar los resultados como mResult.
mResult <- agnes(d, method="ward")
mResult

#R informa un coeficiente de aglomeracion de 0.94

#Usamos la funcion cutree para obtener los clusteres y luego agregamos la informacion de pertenencia al cluster al marco de datos myData. 
mClusters <- cutree(mResult, k=2)
myData <- data.frame(myData, mClusters)

#Usamos la funcion plot para obtener el grafico del dendrograma.
plot(mResult, which.plots =2) 
fviz_dend(mResult, cex = 0, k = 2, main="Dendograma aglomeramiento jerárquico",
          color_labels_by_k = FALSE, k_colors=c("#2A2640","#17A67D"))

#Usamos la funcion de resumen para obtener estadisticas de resumen para cada grupo.
summary(subset(myData, mClusters==1))
summary(subset(myData, mClusters==2))

#Para averiguar el numero de observaciones en cada grupo, usamos la funcion as.factor para convertir mCluster en datos categoricos 
#y luego usamos la funcion de resumen para averiguar el numero de observaciones en cada grupo. 
summary(as.factor(mClusters))

#Usamos la funcion cbind para conocer que individuos pertenecen a cada cluster.
Cluster <- cbind(Surveyed, mClusters)

#Generamos un data frame con la tabla de datos. 
Cluster_f <- data.frame(Cluster)
Cluster_f

#Exportamos el data frame a un archivo xlsx.
write.xlsx(Cluster_f,"Cluster_f.xlsx", asTable = TRUE) 