public class WrapperTest{

    public static void printErrror(String fctNom,String message, int resultatAttendu, int resultatReel){
        System.out.println("Erreur dans la fonction : " + fctNom);
        System.out.println(message);
        System.out.println("resultat attendu : " + resultatAttendu);
        System.out.println("resultat reel : " + resultatReel);
    }

    public static void printErrror(String fctNom, int resultatAttendu, int resultatReel){
        System.out.println("Erreur dans la fonction : " + fctNom);
        System.out.println("resultat attendu : " + resultatAttendu);
        System.out.println("resultat reel : " + resultatReel);
    }

    public static void main(String[] args){
        int x = 1;
        int y = 2;

        if(MyClass.fstProj(x,y) != x)
            printErrror("fstProj","les parametres ne s'empilent pas dans le bon ordre"
                        ,1,MyClass.fstProj(x,y));
        else
            System.out.println("fstProj : OK");

        if(MyClass.scdProj(x,y) != y)
            printErrror("scdProj","les parametres ne s'empilent pas dans le bon ordre",
                        2,MyClass.scdProj(x,y));
        else
            System.out.println("scdProj : OK");

        if(MyClass.fibo(10) != 55)
            printErrror("fibo",55,MyClass.fibo(10));
        else System.out.println("fibo : OK");
    }
}
