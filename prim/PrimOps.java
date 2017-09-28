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
import eta.runtime.exception.EtaException;
import eta.runtime.concurrent.Concurrent;
import eta.runtime.concurrent.MVar;

import static ghc_prim.ghc.Types.*;
import static eta.runtime.stg.TSO.WhatNext.*;

/* TODO: Provide cleanup operations by extending the runtime with hooks. */

public class PrimOps {
    /* We do not need to worry about thread-safety since only one-thread at a
       time has a given TSO - a guarantee provided by the Eta RTS. */
    public static final Map<TSO, Stack<Closure>>
        tsoContStack   = new IdentityHashMap<TSO, Stack<Closure>>();

    public static final Map<TSO, Closure>
        tsoCurrentCont = new IdentityHashMap<TSO, Closure>();

    public static void setCurrentC(StgContext context, Closure action) {
        context.currentTSO.currentCont = action;
    }

    public static void pushNextC(StgContext context, Closure action) {
        context.currentTSO.contStack.push(action);
    }

    public static Closure popNextC(StgContext context) {
        return context.currentTSO.contStack.pop();
    }

    public static Closure getCurrentC(StgContext context) {
        return context.currentTSO.currentCont;
    }

    public static Stack<Closure> getContStack(StgContext context) {
        return context.currentTSO.contStack;
    }

    public static Closure popContStack(StgContext context, Stack<Closure> stack) {
        if (stack.empty()) {
            context.I1 = 0;
            return null;
        } else {
            context.I1 = 1;
            return stack.pop();
        }
    }

    public static void yieldWith(StgContext context, Closure fiber, int block) {
        TSO tso = context.currentTSO;
        tso.closure = Closures.evalLazyIO(fiber);
        tso.whatNext = (block == 1)? ThreadBlock : ThreadYield;
    }

    public static Closure raise(StgContext context, Closure exception) {
        throw new EtaException(exception);
    }

    public static void addMVarListener(StgContext context, MVar m) {
        m.addListener(context.currentTSO);
    }

    public static void awakenMVarListeners(StgContext context, MVar m) {
        TSO tso = null;
        while ((tso = m.grabListener()) != null) {
            Concurrent.pushToGlobalRunQueue(tso);
        }
    }
}
