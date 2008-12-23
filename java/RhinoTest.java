import java.io.*;

import org.mozilla.javascript.*;


public final class RhinoTest {
    public static void main(String[] args) {
        if (args.length > 1) {
            System.err.println("usage: RhinoTest [FILE]");
            System.exit(1);
        }

        try {
            String src = args.length == 1 ? args[0] : "stdin";
            Reader r = args.length == 1 ? new FileReader(new File(args[0]))
                                        : new InputStreamReader(System.in);

            ContextFactory cf = new ContextFactory(); // I hate factories

            Context c = cf.enterContext();
            Script  s = c.compileReader(r, src, 1, null);

            System.out.println(c.decompileScript(s, 4));
        } catch (IOException e) {
            System.err.println("IO error");
            System.exit(1);
        }
    }
}
