package moscow.ptnl.contingent.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Deprecated() //Перенесен в Common
public class TransactionRunner {

    @Transactional(propagation = Propagation.REQUIRED)
    public void run(Runnable runnable) {
        runnable.run();
    }
}
