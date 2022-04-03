#Analisis de componentes principales
x< Quitar los espacios de los nombres
colnames(x)[4]="Life.Exp"
colnames(x)[6]= "HS.Grad"

# Se definen n (numero de estados) y p (variables)
dim(x)

n<-dim(x)[1]
p<-dim(x)[2]

# Generación de un scatterplot

pairs(x,col="blue", pch=19, 
      main="Variables originales")

#Obtención de los componentes principales con la matriz de covarianza muestral
mu<-colMeans(x)
s<-cov(x)

# Obtención de los componentes principales con la matriz de covarianza muestral
es<-eigen(s)
es

# Matriz de autovalores
eigen.val<-es$values

# Matriz de autovectores
eigen.vec<-es$vectors

# Proporción de variabilidad para cada vector
pro.var<-eigen.val/sum(eigen.val)

# Proporción de variabilidad acumulada
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)


# Obtencion de los componentes principales con la matriz de correlaciones muestrales
R<-cor(x)
eR<-eigen(R)
eR

# Obtención de auto-valores
eigen.val<-eR$values

# Obtención de auto-vectores
eigen.vec<-eR$vectors

# Proporcion de variablidad
pro.var<-eigen.val/sum(eigen.val)

# Proporcion de variabilidad acumulada
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)

# Media de los auto-valores
mean(eigen.val)

# Obtencion de los coeficientes (nuevas variables)

#Centrar los datos con respecto a la media
ones<-matrix(rep(1,n),nrow=n, ncol=1)

# Construccion de la matriz centrada
X.cen<-as.matrix(x)-ones%*%mu
X.cen

#  Construccion de la matriz diagonal de las varianzas
Dx<-diag(diag(s))
Dx

#Construccion de la matriz centrada multiplicada por Dx^1/2

Y<-X.Cen%*%solve(Dx)^(1/2)
Y 

# Construccion de los coeficientes 

# eigen.vec matriz de autovectores
scores<-Y%*%eigen.vec

# Nombramos las columnas PC1...PC8
colnames(scores)<-c("PC1","PC2","PC3","PC4","PC5",
                    "PC6", "PC7","PC8")

# visualizamos
scores

# Generacion del grafico de los scores
pairs(scores, main="scores", col="blue", pch=19)


#Aplicar el cálculo de la varianza a las columnas 1=filas, 2=columnas
apply(x, 2, var)

# Centrado por la media y escalada por la desviacion standar

acp<-prcomp(x, center=TRUE, scale=TRUE)
acp

# Generación del gráfico screeplot
plot(acp, type="l")

# Visualizar el resumen
summary(acp)

# Construcción del Biplot
biplot(acp, scale=0