package eta.fibers;

import java.util.Queue;
import java.util.Map;
import java.util.Stack;
import java.util.IdentityHashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Closures;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.TSO;
import eta.runtime.concurrent.Concurrent;
import eta.runtime.concurrent.Fiber;
import eta.runtime.concurrent.MVar;

import static ghc_prim.ghc.Types.*;
import static eta.runtime.stg.TSO.WhatNext.*;
import static eta.runtime.stg.Closures.*;

/* TODO: Provide cleanup operations by extending the runtime with hooks. */

public class PrimOps {

    public static void setCurrentC(StgContext context, Closure action) {
        context.currentTSO.currentCont = action;
    }

    public static void pushNextC(StgContext context, Closure action) {
        context.currentTSO.pushCont(action);
    }

    public static Closure popNextC(StgContext context) {
        return context.currentTSO.popCont();
    }

    public static Closure getCurrentC(StgContext context) {
        return context.currentTSO.currentCont;
    }

    public static Closure popContStack(StgContext context) {
        TSO tso = context.currentTSO;
        if (tso.emptyContStack()) {
            context.I1 = 0;
            return null;
        } else {
            context.I1 = 1;
            return tso.popCont();
        }
    }

    public static Closure resumeFiber = null;

    static {
        try {
          resumeFiber = loadClosure("eta_fibers_dev.control.concurrent.fiber.Internal", "resumeFiber");
        } catch (Exception e) {
            System.err.println("FATAL ERROR: Failed to load resumeFiber closure.");
            e.printStackTrace();
            System.exit(1);
        }
    }

    public static void yieldFiber(StgContext context, int block) {
        TSO tso = context.currentTSO;
        tso.whatNext = (block == 1)? ThreadBlock : ThreadYield;
        Closure oldClosure = tso.closure;
        if (oldClosure instanceof EvalLazyIO) {
            ((EvalLazyIO) oldClosure).p = resumeFiber;
        } else {
            oldClosure = Closures.evalLazyIO(resumeFiber);
        }
        throw Fiber.yieldException.get();
    }

    public static void addMVarListener(StgContext context, MVar m) {
        m.registerListener(context.currentTSO);
    }

    public static void awakenMVarListeners(StgContext context, MVar m) {
        for (TSO top = m.getListeners(); top != null;) {
            Concurrent.pushToGlobalRunQueue(top);
            TSO oldTop = top;
            top = top.link;
            oldTop.link = null;
        }
    }
}
