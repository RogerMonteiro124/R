#Developed By Roger Montiero
#Github: https://github.com/RogerMonteiro124
#Date: 09/05/2017
#Time:02:45:21

#Calcular a probabilidade de uma maquina
#produzir um produto com defeito usando o Teorema de Bayes

#############################################
##              Teorema                    ##
##   Pr(A|B)=(Pr(B|A) Pr(A))/Pr(B)         ##                                           
##                                         ##
#############################################
main<-function(){
  #Maquinas
  #cada maquina possue 2 parametros: A quantidade produzida 
  #com defeito e a quantidade produzida no total
  m1<-c(1,35)
  m2<-c(2,20)
  m3<-c(3,15)
  m4<-c(4,2)
  m5<-c(1,18)
  
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
  
  PD<-function(m1,m2,m3,m4,m5){
      D=PDUMi(m1)+PDUMi(m2)+PDUMi(m3)+PDUMi(m4)+PDUMi(m5)
      return(D)
  }
  D<-PD(m1,m2,m3,m4,m5)
  PM1D<-function(M1,D){
    return(round((PDMi(M1)*PMi(M1))/D*100,1))
  }
  PM2D<-function(M2,D){
    return(round((PDMi(M2)*PMi(M2))/D*100,1))
  }
  PM3D<-function(M3,D){
    return(round((PDMi(M3)*PMi(M3))/D*100,1))
  }
  PM4D<-function(M4,D){
    return(round((PDMi(M4)*PMi(M4))/D*100,1))
  }
  PM5D<-function(M5,D){
    return(round((PDMi(M5)*PMi(M5))/D*100,1))
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
  
  grafico<-c(PM1D(m1,D),PM1D(m2,D),PM1D(m3,D),PM1D(m4,D),PM1D(m5,D))
  graficolabels<-paste(grafico,"%",sep = "")
  
  pie(grafico,main = "Probabilidade de produzir um produto defeituoso.",labels = graficolabels,col = c(4,2,7,6,9))
  
  #Adcionando a Legenda
  
  legend("bottomright","(1,2)", cex=0.8,fill = c(4,2,7,6,9),legend = c("Maquina 1","Maquina 2","Maquina 3","Maquina 5","Maquina 5"))
}
main()


