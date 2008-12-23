import java.io.*;

import org.mozilla.javascript.*;


public final class RhinoTest {
    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("usage: RhinoTest [FILE]");
            System.exit(1);
        }

        try {
            File f = new File(args[0]);
            Reader r = new FileReader(f);

            ContextFactory cf = new ContextFactory(); // I hate factories

            Context c = cf.enterContext();
            Script  s = c.compileReader(r, args[0], 1, null);

            System.out.println(c.decompileScript(s, 4));
        } catch (IOException e) {
            System.err.println("IO error");
            System.exit(1);
        }
    }
}
