package org.example.geom;

public class Point2 {

    private double x;

    private double y;

    //private double z;

    public String toString() {
        /*%%
        return super.toString() + "["
        <%
        var first = true;
        for (var i = 0; i < $module.class.members.length; ++i) {
            var prop = $module.class.members[i];
            if (prop.kind === 'field') { %>
            + "<%= first ? '' : ', ' %><%= prop.name %>=" + this.<%= prop.name %>
            <%
                first = false;
            }
        }
        %>
            + "]";
        %%*/
        //==
        return super.toString() + "["
            + "x=" + this.x
            + ", y=" + this.y
            + "]";
        //==
    }

    public static void main(String[] args) {
        Point2 p = new Point2();
        System.out.println("point: " + p);
    }
}
