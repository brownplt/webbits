import java.io.*;

import org.mozilla.javascript.*;


public final class RhinoTest {
    
    /**
     * lol javadocs
     */
    public static String prettyPrint(Reader rdr, String src) throws IOException {
        ContextFactory cf = new ContextFactory(); // I hate factories

        Context c = cf.enterContext();
        Script  s = c.compileReader(rdr, src, 1, null);

        return c.decompileScript(s, 4);
    }

    public static void main(String[] args) {
        if (args.length > 1) {
            System.err.println("usage: RhinoTest [FILE]");
            System.exit(1);
        }

        try {
            String src;
            Reader rdr;

            if (args.length == 0) {
                src = "stdin";   
                rdr = new InputStreamReader(System.in);
            } else {
                src = args[0];
                rdr = new FileReader(new File(src));
            }

            System.out.println(RhinoTest.prettyPrint(rdr, src));

        } catch (IOException e) {
            System.err.println("IO error");
            System.exit(1);
        }
    }
}
