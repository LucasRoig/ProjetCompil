public class WrapperTest{

    public static void printError(String fctNom,String message, int resultatAttendu, int resultatReel){
        System.out.println("Erreur dans la fonction : " + fctNom);
        System.out.println(message);
        System.out.println("resultat attendu : " + resultatAttendu);
        System.out.println("resultat reel : " + resultatReel);
    }

    public static void printError(String fctNom, int resultatAttendu, int resultatReel){
        System.out.println("Erreur dans la fonction : " + fctNom);
        System.out.println("resultat attendu : " + resultatAttendu);
        System.out.println("resultat reel : " + resultatReel);
    }

    public static void main(String[] args){
        int x = 1;
        int y = 2;

        if(MyClass.fstProj(x,y) != x)
            printError("fstProj","les parametres ne s'empilent pas dans le bon ordre"
                        ,1,MyClass.fstProj(x,y));
        else
            System.out.println("fstProj : OK");

        if(MyClass.scdProj(x,y) != y)
            printError("scdProj","les parametres ne s'empilent pas dans le bon ordre",
                        2,MyClass.scdProj(x,y));
        else
            System.out.println("scdProj : OK");

        if(MyClass.fibo(10) != 55)
            printError("fibo",55,MyClass.fibo(10));
        else System.out.println("fibo : OK");

        if(MyClass.pair(18)==1)
            System.out.println ("pair : ok");
        else printError("pair","Erreur due a un CallE",1,MyClass.pair(18));

	if(MyClass.sumImpair(10)==25)
	    System.out.println("sumImpair : ok");
	else printError("sumImpair",25,MyClass.impair(10));
    }
}
