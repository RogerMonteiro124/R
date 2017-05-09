#Developed By Roger Montiero
#Github: https://github.com/RogerMonteiro124
#Date: 09/05/2017
#Time:02:45:21

#Calcular a probabilidade de uma maquina
#produzir um produto com defeito
main<-function(){
  #Maquinas
  #cada maquina possue 2 parametros: A quantidade produzida 
  #com defeito e a quantidade produzida no total
  m1<-c(1,50)
  m2<-c(2,40)
  m3<-c(3,10)
  
  #Essa funcao calcula a fraçao de vezes que a maquina é usada
  PMi<-function(Mi){
    return(Mi[2]/100)
  }
  
  
  PDMi<-function(Mi){
    return(Mi[1]/100)
  }
  PDUMi<-function(Mi){
    return(PMi(Mi)*PDMi(Mi))
  }
  
  PD<-function(m1,m2,m3){
      D=PDUMi(m1)+PDUMi(m2)+PDUMi(m3)
      return(D)
  }
  D<-PD(m1,m2,m3)
  PM1D<-function(M1,D){
    return((PDMi(M1)*PMi(M1))/D*100)
  }
  PM2D<-function(M2,D){
    return((PDMi(M2)*PMi(M2))/D*100)
  }
  PM3D<-function(M3,D){
    return((PDMi(M3)*PMi(M3))/D*100)
  }
  
  
  ###DEBUGS###
  #PMi(m1)
  #PMi(m2)
  #PMi(m3)
  #PDMi(m1)
  #PDMi(m2)
  #PDMi(m3)
  #PDUMi(m1)
  #PDUMi(m2)
  #PDUMi(m3)
  #PD(m1,m2,m3)
  
  
  #PLOTANDO O GRAFICO
  
  grafico<-c(PM1D(m1,D),PM1D(m2,D),PM1D(m3,D))
  pie(grafico,main = "Probabilidade de produzir um produto defeituoso.(%)",labels =c(PM1D(m1,D), PM1D(m2,D),PM1D(m3,D)),col = c(4,2,7))
  
  #Adcionando a Legenda
  
  legend("topright",fill = c(4,2,7),legend = c("Maquina 1","Maquina 2","Maquina 3"))
}
main()

