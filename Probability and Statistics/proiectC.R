#==========================C1=================================
valNod = function(frunze, nodCautat) 
{
  nrFrunze = length(frunze)
  nrNoduri=nrFrunze+nrFrunze/2;
  ult=1  #ult, retin ult nod de pe nivelul anterior 
  p=1
  nivel=0#nivel radacina
  while(ult<nodCautat)
  {
    nivel=nivel+1
    p=p*3
    ult=ult+p
  }
  #primulNod=nrNoduri-nrFrunze+1#primul nod ult rand si daca 3*i e cuprins intre primulNod si nr Noduri
  if(9*nodCautat>nrNoduri) #nodul se afla pe penultimul nivel, cu formula
  { #copiii-frunze
    if(frunze[3*nodCautat-ult-1] == 0)#fiu stang
    {
      if(frunze[3*nodCautat-ult]==0)#fiu dedesubt
        return(frunze[3*nodCautat-ult+1])#fiu drept
      else
        return(1)
    }
    else
      return(1)
  }
  if(nivel%%2==0) # nodul i e AND, inclusiv radacina
  { 
    #daca gasesc un fiu 0->voi returna 0
    if(valNod (frunze,3*nodCautat-1) == 0)#apelez recursiv fiul din stanga
      return(0)
    if(valNod (frunze,3*nodCautat) == 0)#apelez recursiv fiul de dedesubt
      return(0)
    if(valNod (frunze, 3*nodCautat+1) == 0)#apelez recursiv fiul din dreapta
      return(0)
    return(1)
  }
  if(nivel%% 2 == 1)#nodul e OR
  {
    if(valNod(frunze,3*nodCautat-1) == 0)
    {
      if(valNod(frunze,3*nodCautat)==0)
        return(valNod(frunze,3*nodCautat+1))     
      else
        return(1)
    }
    else
      return(1)
  }
}
#frunze9 = c(1,1,1,0,1,0,1,1,0) 
frunze9 = c(0,0,1,0,0,0,1,1,0) 
radacina=valNod (frunze9,1)
cat("Radacina game tree are valoarea", radacina)
#==========================C2=================================

#==========================C3=================================
number=function(cuvantV){
  suma=0
  lungime=length(cuvantV)
  for(i in 1:lungime)
  {
    suma=suma+v[i]*(2^(i-1))
  }
  return(suma)
}

ex_C3=function(n,cuvantU){
  #p-un nr prim<=n^2;
  p=sample(c(primes::generate_primes(2,n^2)),1)# gener o secv de numere prime intre 2 si n^2 si aleg 1
  r=(number(cuvantU))%%p
  m=length(r)
  for(j in 1:m)
  {
    r[j]=(number(cuvantU^j))%%p
  }
  for(j in 1:m)#verific daca r apartine{r[1],r[2],r[3]....r[m]}
  {
    if(r == r[i] )
      return (TRUE)#u apartine
  }
  return (FALSE)#u nu apartine
}
