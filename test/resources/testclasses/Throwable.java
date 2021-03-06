package java.lang;
import  java.io.*;

public class Throwable implements Serializable {
    // private static final long serialVersionUID = -3042686055658047285L;
    // private transient Object backtrace;
    // private String detailMessage;
    // private Throwable cause = this;
    // private StackTraceElement[] stackTrace;

    // public String getMessage() {
    //     return detailMessage;
    // }
//
    // public String getLocalizedMessage() {
    //     return getMessage();
    // }
//
    // public Throwable getCause() {
    //     return (cause==this ? null : cause);
    // }

    // public void printStackTrace(PrintStream s) {
    //     synchronized (s) {
    //         s.println(this);
    //         StackTraceElement[] trace = getOurStackTrace();
    //         for (int i=0; i < trace.length; i++)
    //             s.println("\tat " + trace[i]);
//
    //         Throwable ourCause = getCause();
    //         if (ourCause != null)
    //             ourCause.printStackTraceAsCause(s, trace);
    //     }
    // }

    // private void printStackTraceAsCause(PrintStream s,
    //                                     StackTraceElement[] causedTrace)
    // {
    //     // assert Thread.holdsLock(s);
//
    //     // Compute number of frames in common between this and caused
    //     StackTraceElement[] trace = getOurStackTrace();
    //     int m = trace.length-1, n = causedTrace.length-1;
    //     while (m >= 0 && n >=0 && trace[m].equals(causedTrace[n])) {
    //         m--; n--;
    //     }
    //     int framesInCommon = trace.length - 1 - m;
//
    //     s.println("Caused by: " + this);
    //     for (int i=0; i <= m; i++)
    //         s.println("\tat " + trace[i]);
    //     if (framesInCommon != 0)
    //         s.println("\t... " + framesInCommon + " more");
//
    //     // Recurse if we have a cause
    //     Throwable ourCause = getCause();
    //     if (ourCause != null)
    //         ourCause.printStackTraceAsCause(s, trace);
    // }

    // public void printStackTrace(PrintWriter s) {
    //     synchronized (s) {
    //         s.println(this);
    //         StackTraceElement[] trace = getOurStackTrace();
    //         for (int i=0; i < trace.length; i++)
    //             s.println("\tat " + trace[i]);
//
    //         Throwable ourCause = getCause();
    //         if (ourCause != null)
    //             ourCause.printStackTraceAsCause(s, trace);
    //     }
    // }

    // private void printStackTraceAsCause(PrintWriter s,
    //                                     StackTraceElement[] causedTrace)
    // {
    //     // assert Thread.holdsLock(s);
//
    //     // Compute number of frames in common between this and caused
    //     StackTraceElement[] trace = getOurStackTrace();
    //     int m = trace.length-1, n = causedTrace.length-1;
    //     while (m >= 0 && n >=0 && trace[m].equals(causedTrace[n])) {
    //         m--; n--;
    //     }
    //     int framesInCommon = trace.length - 1 - m;
//
    //     s.println("Caused by: " + this);
    //     for (int i=0; i <= m; i++)
    //         s.println("\tat " + trace[i]);
    //     if (framesInCommon != 0)
    //         s.println("\t... " + framesInCommon + " more");
//
    //     // Recurse if we have a cause
    //     Throwable ourCause = getCause();
    //     if (ourCause != null)
    //         ourCause.printStackTraceAsCause(s, trace);
    // }
//
    //public synchronized native Throwable fillInStackTrace();

    public StackTraceElement[] getStackTrace() {
        return (StackTraceElement[]) getOurStackTrace().clone();
    }

    private synchronized StackTraceElement[] getOurStackTrace() {
        // Initialize stack trace if this is the first call to this method
        // if (stackTrace == null) {
        //     int depth = getStackTraceDepth();
        //     stackTrace = new StackTraceElement[depth];
        //     for (int i=0; i < depth; i++)
        //         stackTrace[i] = getStackTraceElement(i);
        // }
        // return stackTrace;
        return null;
    }

    // native int getStackTraceDepth();
    // native StackTraceElement getStackTraceElement(int index);
//
    // private synchronized void writeObject(java.io.ObjectOutputStream s)
    //     throws IOException
    // {
    //     getOurStackTrace();  // Ensure that stackTrace field is initialized.
    //     s.defaultWriteObject();
    // }
}
