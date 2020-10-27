//%%{ mode = "replace" }%%
package org.example.geom;

public class Point3 {

    private double x;

    private double y;

    //private double z;

    public String toString() {
        /*<@
        #def to_string($type)
            #for $f in $type.fields
                #if $first
        return super.toString() + "["
            + "<% $f.name %>=" + this.<% $f.name %>
                #else
            + ", <% $f.name %>=" + this.<% $f.name %>
                #end
                #if $last
            + "]";
                #end
            #else
        return super.toString();
            #end
        #end
        @>*/
        //@@ #print to_string($class, 1, 'ddd')
        return super.toString() + "["
            + "x=" + this.x
            + ", y=" + this.y
            + "]";
        //@@ #end

    }

    public static void main(String[] args) {
        Point3 p = new Point3();
        System.out.println("point: " + p);
    }
}
