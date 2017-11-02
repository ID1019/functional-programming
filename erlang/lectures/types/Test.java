
public class Test {
    
    public static int fibonacci(int n) {

        int prevprev = 0;
	int prev = 1;
	int current = 0;

	for(int i = 0; i < n; i++) { 
	    System.out.format("fib(%d) = %d%n", i, current);
	    current = prev + prevprev;  
	    prevprev = prev;
	    prev = current; 
	}
	System.out.format("fib(%d) = %d%n", n, current);
	return current;
    };

    public static void main(String[] args) {
	int fib20;

	fib20 = fibonacci(20);

	System.out.format("hello %d%n", fib20);
    };
 

}



