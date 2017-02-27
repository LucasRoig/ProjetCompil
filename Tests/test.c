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
