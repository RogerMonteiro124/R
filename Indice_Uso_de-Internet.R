#Mostra o indice de Intenet no mundo
#By Roger Montiero
main<-function (){
  library(readr)
  Indice <- read_csv("~/R/Indice.csv")
  porcentagem<-round(Indice$`indice_velocidade_internet(mbps)`
                *100/sum(Indice$`indice_velocidade_internet(mbps)`))
  rotulo<-paste(Indice$Pais,"(",porcentagem," MB/s)",sep="")
  pie(Indice$`indice_velocidade_internet(mbps)`
      ,main = "Indice de velocidade de internet",labels = rotulo,col=rainbow(7))
}
main()
