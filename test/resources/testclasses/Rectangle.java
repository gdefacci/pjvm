public class Rectangle {

  public static class Point {
    public static final Point ZERO = new Point(0,0);

    public static Point at(int x, int y) {
      return new Point(x,y);
    }

    public final int x;
    public final int y;

    public Point(int x, int y) {
      this.x = x;
      this.y = y;
    }

    public Point withX(int x) {
      return new Point(x,y);
    }
    public Point withY(int y) {
      return new Point(x,y);
    }
  }

  public final Point tl;
  public final Point tr;
  public final Point br;
  public final Point bl;

  public Rectangle(Point br) {
    this(Point.ZERO, br.withY(0), br, br.withX(0));
  }

  public Rectangle(Point tl, Point tr, Point br, Point bl) {
    this.tl = tl;
    this.tr = tr;
    this.br = br;
    this.bl = bl;
  }

}