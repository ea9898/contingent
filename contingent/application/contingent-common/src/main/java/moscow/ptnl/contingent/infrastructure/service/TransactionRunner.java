package moscow.ptnl.contingent.infrastructure.service;

import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.AsyncResult;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.concurrent.Callable;
import java.util.concurrent.Future;

@Service
public class TransactionRunner {

    @Async
    @Transactional(propagation = Propagation.REQUIRED, rollbackFor = Throwable.class)
    public <T> Future<T> run(Callable<T> callable) {
        try {
            return new AsyncResult<>(callable.call());
        }
        catch (Throwable ex) {
            throw new RuntimeException("Transaction has thrown an Exception", ex);
        }
    }
}
