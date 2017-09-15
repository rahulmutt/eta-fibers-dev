package eta.fibers;

import java.util.Stack;
import java.util.IdentityHashMap;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.TSO;

import static ghc_prim.ghc.Types.*;

/* TODO: Provide cleanup operations by extending the runtime with hooks. */

public class PrimOps {
    /* We do not need to worry about thread-safety since only one-thread at a
       time has a given TSO - a guarantee provided by the Eta RTS. */
    public static final IdentityHashMap<TSO, Stack<Closure>>
        tsoContStack = new IdentityHashMap<TSO, Stack<Closure>>();

    public static final IdentityHashMap<TSO, Closure>
        tsoCurrentCont = new IdentityHashMap<TSO, Closure>();

    public static Closure setCurrentC(StgContext context, Closure action) {
        tsoCurrentCont.put(context.currentTSO, action);
        return null;
    }

    public static Closure pushNextC(StgContext context, Closure action) {
        Stack<Closure> contStack = tsoContStack.get(context.currentTSO);
        if (contStack == null) {
            contStack = new Stack<Closure>();
            tsoContStack.put(context.currentTSO, contStack);
        }
        contStack.push(action);
        return null;
    }

    public static Closure popNextC(StgContext context) {
        return tsoContStack.get(context.currentTSO).pop();
    }

    public static Closure getCurrentC(StgContext context) {
        return tsoCurrentCont.get(context.currentTSO);
    }

    public static Closure getContStack(StgContext context) {
        Stack<Closure> contStack =
            (Stack<Closure>) tsoContStack.get(context.currentTSO).clone();
        ZCD next = new ZCD(null, DZMZN());
        for (Closure c: contStack) {
            next.x1 = c;
            next = new ZCD(null, next);
        }
        return next.x2;
    }
}
