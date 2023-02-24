#========================E1========================
ex_E1 = function(n, medProd, medSelectie, dev, alfa, Ha){
  
  t = (medSelectie - medProd)/(dev/sqrt(n));#scor
  if(Ha=='as_stanga'){
    valCriticaT = qt(alfa,n-1);
    if(t < valCriticaT){
      cat("ipoteza nula este respinsa,ipoteza alternativa acceptata \n");
    }
    else{
      cat("Nu sunt suficiente dovezi pentru a respinge ipoteza nula \n");
    }
  }
  else if(Ha=='as_dreapta'){
    valCriticaT = qt(1 - alfa,n-1);
    if(t > valCriticaT){
      cat("ipoteza nula este respinsa,ipoteza alternativa acceptata \n");
    }
    else{
      cat("Se accepta ipoteza nula")
    }
  }
  else#ipoteza simetrica
   { valCriticaT = qt(1 - alfa/2,n-1);
    if(abs(t) > abs(valCriticaT)){
      cat("ipoteza nula este respinsa,ipoteza alternativa acceptata \n");
    }
    else{
      cat("Se accepta ipoteza nula")
    }
  }
  cat("Scorul:", t," Valoarea critica: ", valCriticaT, "\n");
}
ex_E1(125,420,418,2.75,0.01,"as_stanga");
#========================E2========================

ex_E2 = function(n, med, medSelectie, sigma, alfa, Ha){
  
  z = (medSelectie - med)/(sigma/sqrt(n));
  if(Ha=='as_stanga'){
    valCriticaZ = qnorm(alfa,0,1);
    if(z < valCriticaZ){
      cat("ipoteza nula este respinsa,ipoteza alternativa acceptata \n");
    }
    else{
      cat("Se accepta ipoteza nula")
    }
  }
  else if(Ha=='as_dreapta'){
    valCriticaZ = qnorm(1 - alfa,0,1);
    if(z > valCriticaZ){
      cat("ipoteza nula este respinsa,ipoteza alternativa acceptata \n");
    }
    else{
      cat("Se accepta ipoteza nula")
    }
  }
  else{
    valCriticaZ = qnorm(1 - alfa/2);
    if(abs(z) > abs(valCriticaZ)){
      cat("ipoteza nula este respinsa,ipoteza alternativa acceptata \n");
    }
    else{
      cat("Se accepta ipoteza nula")
    }
  }
  cat("Scorul:", z," Valoarea critica: ", valCriticaZ, "\n");
}
ex_E2(25, 4.9,5.17,0.35,0.01,"sim");
ex_E2(25, 4.9,5.17,0.35,0.05,"as_dreapta");
#========================E3========================

ex_E3 = function(nx,ny,medSelectiex, medSelectiey,sigmax,sigmay, alfa, Ha){
  
  sigma=sqrt(sigmax^2/nx+sigmay^2/ny)
  z = (medSelectiex - medSelectiey)/sqrt((sigmax^2)/nx+(sigmay^2)/ny);
  if(Ha=='as_stanga'){
    valCriticaZ = qnorm(alfa,0,1);
    if(z < valCriticaZ){
      cat("ipoteza nula este respinsa,ipoteza alternativa acceptata \n");
    }
    else{
      cat("Se accepta ipoteza nula")
    }
  }
  else if(Ha=='as_dreapta'){
    valCriticaZ = qnorm(1 - alfa,0,1);
    if(z > valCriticaZ){
      cat("ipoteza nula este respinsa,ipoteza alternativa acceptata \n");
    }
    else{
      cat("Se accepta ipoteza nula")
    }
  }
  else{
    valCriticaZ = qnorm(1 - alfa/2);
    if(abs(z) > abs(valCriticaZ)){
      cat("ipoteza nula este respinsa,ipoteza alternativa acceptata \n");
    }
    else{
      cat("Se accepta ipoteza nula")
    }
  }
  cat("Scorul:", z," Valoarea critica: ", valCriticaZ, "\n");
}
subp_a=ex_E3(25,28, 5.48, 6.12,1.31,0.93,0.01,'sim');

subp_b=ex_E3(75, 87,20.5, 21.6, 1.15, 0.92, 0.01, 'as_stanga');
#========================E4========================

ex_E4 = function(nx,ny,sx,sy, alfa, Ha){
  f = sx^2/sy^2
  if(Ha=='as_dreapta'){
    valCriticaF = qf(1 - alfa,nx-1,ny-1);
    if(f > valCriticaF){
      cat("ipoteza nula este respinsa,ipoteza alternativa acceptata \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula, nu sunt suficiente dovezi \n");
    }
    cat("Scorul:", f," Valoare critica: ", valCriticaF, "\n");
  }
  else if(Ha=='sim')
  {
    ValCriticaFS = qf(alfa/2,nx-1,ny-1);
    ValCriticaFD = qf(1-alfa/2,nx-1,ny-1);
    if((f<ValCriticaFS )||(f>ValCriticaFD)){
      cat("ipoteza nula este respinsa,ipoteza alternativa acceptata \n");
    }
    else{
      cat("Nu se poate respinge ipoteza nula, nu sunt suficiente dovezi \n");
    }
    cat("Scorul:", f," Valori critice: ", ValCriticaFS, ValCriticaFD,  "\n");
  }
}
ex_E4(25,28,1.24,0.87,0.01,"as_dreapta");
