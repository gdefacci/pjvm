class SelfCall {
  
  public static void run(Runnable r) {
    r.run();
  }
  
}