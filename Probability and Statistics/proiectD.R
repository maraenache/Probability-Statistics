#========================D1========================
ex_D1=function(n, med, p, dev)
{
  alfa=1-p
  sigma = sqrt(dev)
  zStar = qnorm(1-alfa/2,mean=0,sd=1) 
  
  a=med-zStar*sigma/sqrt(n)#Z*=critical_Z
  b=med+zStar*sigma/sqrt(n)#L5III
  
  return (c(a,b))#intervalul de incredere
}
ex_D1(100,300,0.9,30)
ex_D1(39,300,0.95,30)

#========================D2========================

ex_D2=function(val)
{
  aparitii95=0
  aparitii99=0
  for(i in 1:100)
  {
    x = sample(0:9,40,replace = TRUE)
    #esantion cu 40 de numere cu o singura cifra
    med = mean(x)
    dev = sd(x)
    interval95 = ex_D1(40,med,0.95,dev)
    interval99 = ex_D1(40,med,0.99,dev)
    if(val>interval95[1] && val<interval95[2])
      #daca val apartine intervalului de incredere de 95% pentru medie
      aparitii95=aparitii95 + 1
    if(val>interval99[1] && val <interval99[2])
      aparitii99=aparitii99 + 1   

  }
  #nraparitii<=100,compar de 100 de ori 
  cat("Nr aparitii pt interval de incredere 95%",aparitii95,
      "\nNr aparitii pt interval de incredere 99%",aparitii99)
}
ex_D2(4.5)
#========================D3========================
ex_D3=function(n,alfa,p0,xSuccese,Ha)#o variabila X ce numara succesele din n incercari. X este distribuita binomial B(n, p).
{
  pPrim=xSuccese/n #frecventa data de esantion
  
  #se formuleaza ipoteza nula H0, care sustine ca probabilitatea p ia valoarea p0
  z = (pPrim-p0)/(sqrt(p0*(1-p0)/n));

  #se formuleaza ipoteza alternativa care poate fi de 3 feluri
  if(Ha=='as_stanga')#asimetrica la stanga p<p0,h_alternativ
  {
    valCriticaZ=qnorm(alfa,0,1)
    if(z<valCriticaZ)#z<z*
     { cat("ipoteza nula H0 este respinsa,ipoteza alternativa Ha acceptata")}
    else
      {cat("Se accepta ipoteza nula")}
  }
  else if(Ha=='as_dreapta')
  {
    valCriticaZ=qnorm(1-alfa,0,1)                          #qnorm: quantile function of the normal distribution, se da o arie si det limitele val determinate de arie
    if(z>valCriticaZ)
      {cat("ipoteza nula H0 este respinsa,ipoteza alternativa Ha acceptata")}
    else
      {cat("Se accepta ipoteza nula")}
  }
  else#ipoteza simetrica
  {
    valCriticaZ=qnorm(1-alfa/2,0,1)
    if(abs(z)>abs(valoareaCriticaZ))
      {cat("ipoteza nula H0 este respinsa, ipoteza alternativa Ha acceptata")}
    else
      {cat("Se accepta ipoteza nula")}
  }
}
ex_D3(1250,0.01,0.72,852,"as_stanga")               
ex_D3(1250,0.05,0.72,852,"as_stanga")
#sau v2--------------
ex_D3_v2=function(n,alfa,p0,xSuccese)
{
  pPrim=xSuccese/n 
  z = (pPrim-p0)/(sqrt(p0*(1-p0)/n));
  valCriticaZ=qnorm(alfa,0,1)
  if(z<valCriticaZ)
  {
    cat("ipoteza nula H0 este respinsa,ipoteza alternativa Ha acceptata")
  }
  else
  {
    cat("Se accepta ipoteza nula")
  }
}
ex_D3_v2(1250,0.01,0.72,852)
#========================D4========================
ex_D4=function(n,alfa,p0,xSuccese,Ha)
{
  ex_D3(n,alfa,p0,xSuccese,Ha)
}
ex_D4(1020,0.01,0.60,623,"as_dreapta")
ex_D4(1020,0.05,0.60,623,"as_dreapta")
#sau v2-----------
ex_D4_v2=function(n,alfa,p0,xSuccese)
{
  pPrim=xSuccese/n 
  z = (pPrim-p0)/(sqrt(p0*(1-p0)/n));
  valCriticaZ=qnorm(1-alfa,0,1)                          
  if(z>valCriticaZ)
  {
    cat("ipoteza nula H0 este respinsa,ipoteza alternativa Ha acceptata")
  }
  else
  {
    cat("Se accepta ipoteza nula")
  }
}
ex_D4_v2(1020,0.01,0.60,623)
