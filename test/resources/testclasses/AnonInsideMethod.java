public class AnonInsideMethod {
  
  public static void myMethod(final String txt) {
    new Runnable() {
      public void run() {
          System.out.println(txt);
      }
    };
  }
  
}

