/* Permet de tester que l'on empile les parametres dans le bon ordre */
int fstProj(int x,int y){
  return x;
}

int scdProj(int x,int y){
  return y;
}

int fibo(int n){
  int x;int y; int z;int aux;
  x = 0;
  y = 1;
  z = 0;
  while(z < n){
    z = z + 1;
    aux = x + y;
    x = y;
    y = aux;
  }
  return x;
}

bool pair(int n){
  if(n==0){return true;}
  return impair(n-1);
}

bool impair(int n){
  if(n==0){return false;}
  return pair(n-1);
}

int sumImpair(int n){
  int sum;
  sum = 0;
  while(n > 0){
    if(impair(n)){
      sum = sum + n;
    }
    n = n-1;
  }
  return sum;
}

void error (int n){
  int x;
  return;
}
