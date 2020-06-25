package moscow.ptnl.contingent.infrastructure.service;

import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.AsyncResult;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Service
public class TransactionRunService {
    
    private final static Logger LOG = LoggerFactory.getLogger(TransactionRunService.class);

    @Async
    @Transactional(propagation = Propagation.REQUIRED, rollbackFor = Throwable.class)
    public <T> Future<T> run(Callable<T> callable) {
        try {
            return new AsyncResult<>(callable.call());
        }
        catch (Throwable ex) {
            LOG.error("Ошибка выполнения асинхронной транзакции", ex);
            throw new RuntimeException("Transaction has thrown an Exception", ex);
        }
    }
}
