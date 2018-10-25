import java.util.Vector;
import java.util.Iterator;

public class TT {
  public static void main(java.lang.String[] unused) {
    Vector v = new Vector();
    Iterator iter = v.iterator();
    while (iter.hasNext()) {
      Object o = iter.next();
      Object o2 = o;
    }
    for (Object o: v) {
      Object o2 = o;
    }
  }
};