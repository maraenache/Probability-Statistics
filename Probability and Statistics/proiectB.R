#=========================B1=========================
ex_B1=function(N)
{
  a=4
  b=3
  c=4
  h=4
  N_C=0
  for(i in 1:N)
  {
     x=runif(1,(-a*sqrt(h/c)),(a*sqrt(h/c)))
     y=runif(1,(-b*sqrt(h/c)),(b*sqrt(h/c)))
     z=runif(1,0,h)
     
     if((x*x)/(a*a)+(y*y)/(b*b)<=(z/c))
        N_C=N_C+1
  }
  volumMonteCarlo=(8*a*b*h*h*N_C)/(N*2*c)
  valoareExacta=(pi*a*b*h*h)/(2*c)
  eroareRelativa=abs((volumMonteCarlo-valoareExacta)/abs(valoareExacta))
  cat("Valoare estimata=",volumMonteCarlo,"; Valoarea exacta=",
      valoareExacta,"; Eroare relativa=",eroareRelativa, "; Diferenta=", abs(volumMonteCarlo-valoareExacta))#L3I
}
esantion1=ex_B1(20000)
esantion2=ex_B1(50000)
esantion3=ex_B1(100000)
  
#=========================B2=========================

ex_B2=function(a,b,c,d,N)
{
  N_C=0
  for(i in 1:N) {
    x = runif(1, a, b)
    y = runif(1, c, d)
    if(x>=1 && y<=2 && y<=x-1 && y<=7-x && y>=0) 
      N_C=N_C+1
    }
    aria=(abs((b-a)*(d-c)*N_C)/N)
    return (aria)
}
ex_B2(2,9,1,10,20000)
#=========================B3=========================
ex_B3a=function(N)
{
  sum=0
  for(i in 1:N)
  {
    u=runif(1,1,2)#integrala de la 1 la 2
    sum=sum+u/((u*u+2)^3)
  }
  cat("Valoare estimata=",((2-1)*sum)/N,"; Valoare exacta=",1/48)
}
ex_B3a(20000)
#---------------------
ex_B3b=function(N)
{
  sum=0
  for(i in 1:N)
  {
    u=runif(1,-3,3)#integrala de la -3 la 3
    sum=sum+1/(u*u+9)
  }
  cat("Valoare estimata=",((3+3)*sum)/N,"; Valoare exacta=",pi/6)
}
ex_B3b(20000)
#---------------------
ex_B3c=function(N)
{
  sum=0
  for(i in 1:N)
  {
    u=rexp(1,1)#distributie exponentiala lambda=1
    sum=sum+u*exp(-u*u)/exp(-u)#L3II /(lambda*exp(-lambda*u))
  }
  cat("Valoare estimata=",(sum)/N,"; Valoare exacta=",1/2)
}
ex_B3c(20000)
#=========================B4=========================
ex_B4=function(N)
{
  timpClient=0;
  for(i in 1:N)
  {
    latenta=rexp(1,4)#lambda=4 
    timpServer1=rgamma(1,4,3)
    timpServer2=rgamma(1,4,2)
    timpServer3=rgamma(1,5,2)
    timpServer4=rgamma(1,5,3)
    probab=runif(1,0,1)
    if(probab<=0.25)#prob 0.25
      {timpClient=timpClient+timpServer1}
    else if(probab<=0.5)#>0.25&&<0.5-prob 0.25
      {timpClient=timpClient+timpServer2}
    else if(probab<=0.8)#>0.5&&<0.8-prob 0.3
      {timpClient=timpClient+timpServer3}
    else #prob 0.2
      {timpClient=timpClient+timpServer4}
    
    timpClient=timpClient+latenta;
  }
  return (timpClient/N);
}
ex_B4(10000)
#=========================B5=========================

conturi=function(n){
  conturiStud=vector(mode="logical",length = n)
  i=sample(n,1) #gen 1 nr intreg intre 1 si n
  conturiStud[i]=TRUE #1 infectat
  return(conturiStud)
}
conturi(50)

#functie care numara conturile infectate

conturiInf=function(conturiStud,N){
  count=0
  for(i in 1:N){
    if(conturiStud[i]==TRUE){
      count=count+1
    }
  }
  return(count)
}
conturiInf(conturi(50),50)

#infectam cu probabilitate p, p poate fi 0.05,0.1,0.2
infectare=function(conturiStud,p){
  
  nr=length(conturiStud)
  for(i in 1:nr){
       if(conturiStud[i]==FALSE)
         { prob=runif(1,0,1)
          #print(prob)
          if(prob<=p){
            conturiStud[i]=TRUE}
         }
  }
  return(conturiStud)
}


#functie de curatare pentru 8 calculatoare

curatare8=function(conturi){
  pozInf=vector()
  m=0
  for(i in 1:length(conturi)){
    if(conturi[i]==TRUE){
      m=m+1
      pozInf[m]=i
    }
  }
  #in pozInf pun pozitiile exacte pe care se gasesc calculatoarele infectate
  # print(curatat)
  nrInf=length(pozInf)#cate sunt infectate
  if(nrInf>=8){nrCurat=8}#curat 8 daca am de unde
  else {nrCurat=nrInf }#daca nu, cate sunt
  pozC=sample(nrInf,nrCurat,replace = F) #de la 1:nr pana la cate sunt ia cate sunt de curatat si face un vect de poz
  #print(index)
  for(i in 1:nrInf){
    conturi[pozInf[pozC[i]]]=FALSE
  }
  return(conturi)
}
conturi(50)

c=conturi(50)
infectare(c,0.2)
curatare8(c)
curatare8(conturi(50))
curatare8(infectare(conturi(50)))




#teste functii
cont=conturi(50)
cont
cont=infectare(cont,0.1)
cont
cont=curatare8(cont)
cont
nrc=conturiInf(cont,50)
nrc

#exercitiul
ex_B5=function(p){
  nrzile=1
  conturiS=conturi(50)
  nrinf=1
  while(nrinf>0)
  {
    conturiS=infectare(conturiS,p)
    #cat(conturiS)
    nrinf = conturiInf(conturiS,50)
    #print(nrinf)
    conturiS=curatare8(conturiS)
    nrinf=conturiInf(conturiS,50)
    if(nrinf==0)
    {
      return(nrzile)
    }
    nrzile=nrzile+1
  }
  cat("nrZile",nrzile)
  return (nrZile)
}

ex_B5(0.2)# 93 17 102 1
ex_B5(0.1)
ex_B5(0.05)