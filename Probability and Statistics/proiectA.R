#==========================A1==========================
ex_A1=function(lambda,p,n,k)
{
  listaPoisson=seq(k,k+n,1)
  distPoisson=dpois(listaPoisson,lambda);#sau dpois(k:(k+n), lambda))
  #barplot(distPoisson,density=30,scale=2,main="Poisson distribution",col="blue",border=NA)
  #par(new=TRUE)

  listaGeom=seq(k,k+n,1)
  distGeom=dgeom(listaGeom,p)
  #barplot(distGeom,scale=0,density=40,main="Geometric distribution",col="red",border=NA)

  listaBinom=seq(0,n,1)
  distBinom=dbinom(listaBinom,n,p)
  #barplot(distBinom,scale=0,density=50,sub="Binomial distribution",col="yellow",border=NA)
  
  acelasiGrafic=cbind(distPoisson,distGeom,distBinom)
  barplot(acelasiGrafic,main="functii de masa de probabilitate\nPoisson(lambda), Geometric(p), B(n,p) ",
          beside=T,sub="reprezentare ex_A1", col=rgb(0.7,0.2,0.5,0.7))
  #acelasiGrafic2<-data.frame(distPoisson,distGeom,distBinom)
  #barplot(as.matrix(acelasiGrafic2),beside=T,col="pink")
}
ex_A1(15.2,0.2,17,4)
#ex_A1(12,0.3,20,2)

#==========================A2==========================

ex_A2a=function(fisier)
{  
  note=scan(fisier)
  statistici=vector()
  statistici[1]=median(note)#mediana
  statistici[2]=mean(note)#media
  statistici[3]=sd(note)#deviatia 

  statistici[4]=as.vector(quantile(note))[1+1]  #quartila Q1 
  statistici[5]=as.vector(quantile(note))[2+1]  #quartila Q2    Qi->[i+1],laborator2
  statistici[6]=as.vector(quantile(note))[3+1]  #quartila Q3 
  return(statistici)
}
#fis trebuie pus in calea indicata de getwd()
getwd()
ex_A2a("notePS.txt")
#-----------------------
ex_A2b=function(fisier)
{
  #valori aberante- metoda cu media (L2III)
  note=scan(fisier)
  nr=0#nr de valori care nu sunt aberante
  
  media=mean(note)
  deviatia=sd(note)
  limSt=media-2*deviatia
  limDr=media+2*deviatia
    
  valNeab=vector() 
  for(i in 1:length(note))
  {
    if(note[i]>limSt&&note[i]<limDr)
    {
      nr=nr+1
      valNeab[nr]=note[i]
    }
  }
  #limSt
  #limDr
  return(valNeab)
}
ex_A2b("notePS.txt")
#-----------------------
ex_A2c=function(fisier)
{
  noteRez=ex_A2b(fisier)
  interval = seq(0, 70, 10)
  hist(noteRez,breaks=interval,xlab ='interval',ylab='frecventa',
       main="distributie frecvente\n -note din esantionul curatat-",
       freq=F,right=T,col='pink')
}
ex_A2c("notePS.txt")

