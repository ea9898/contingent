package ru.lanit.emias2.contingent.state.cache;

import jakarta.transaction.TransactionManager;

/**
 * Created by mnasyrov on 29 июня 2016.
 */
public class CacheTransactionScope {
    private final TransactionManager tx;
    private final boolean transactionStarted;
    CacheTransactionScope(TransactionManager tx) {
        this.tx = tx;
        this.transactionStarted = begin();
    }

    private boolean begin() {
        if(tx == null)
            return false;
        try {
            if(tx.getTransaction() != null)
                return false;

            tx.begin();
            return true;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public void commit() {
        if(!transactionStarted)
            return;
        try {
            tx.commit();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public void rollback() {
        if(!transactionStarted)
            return;
        try {
            tx.rollback();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
