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

    public static final Map<Integer, Queue<TSO>>
        mvarListeners = new ConcurrentHashMap<Integer, Queue<TSO>>();

    public static final AtomicBoolean mvarListenersLock
        = new AtomicBoolean();

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

    public static Closure yieldWith(StgContext context, Closure fiber, int block) {
        TSO tso = context.currentTSO;
        tso.closure = Closures.evalLazyIO(fiber);
        tso.whatNext = (block == 1)? ThreadBlock : ThreadYield;
        tsoContStack.put(tso, null);
        tsoCurrentCont.put(tso, null);
        return null;
    }

    public static Closure raise(StgContext context, Closure exception) {
        throw new EtaException(exception);
    }

    public static Closure addMVarListener(StgContext context, MVar m) {
        Queue<TSO> listeners = null;
        int mvarHash = m.hashCode();
        TSO tso = context.currentTSO;
        do {
            listeners = mvarListeners.get(mvarHash);
            if (listeners == null) {
                if (mvarListenersLock.compareAndSet(false,true)) {
                    try {
                        listeners = new ConcurrentLinkedQueue();
                        listeners.offer(tso);
                        mvarListeners.put(mvarHash, listeners);
                    } finally {
                        mvarListenersLock.set(false);
                    }
                } else continue;
            } else {
                listeners.offer(tso);
                break;
            }
        } while (listeners == null);
        return null;
    }

    public static Closure awakenMVarListeners(StgContext context, MVar m) {
        Queue<TSO> listeners = mvarListeners.get(m.hashCode());
        if (listeners != null) {
            TSO tso = null;
            while ((tso = listeners.poll()) != null) {
                Concurrent.pushToGlobalRunQueue(tso);
            }
        }
        return null;
    }
}
