package moscow.ptnl.contingent.service.esu;

import moscow.ptnl.contingent.domain.esu.EsuInput;
import moscow.ptnl.contingent.repository.esu.EsuInputCRUDRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import ru.mos.emias.esu.lib.consumer.EsuConsumerMessageProcessor;
import ru.mos.emias.esu.lib.consumer.EsuMessage;

import java.lang.invoke.MethodHandles;
import java.time.LocalDateTime;

@Component
@Transactional(propagation = Propagation.REQUIRED)
public class EsuConsumerDatabaseProcessor extends EsuConsumerMessageProcessor {

    private static final Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    @Autowired
    private EsuInputCRUDRepository esuInputCRUDRepository;

    @Override
    public void process(EsuMessage esuMessage) {
        LOG.info(String.format("Получено сообщение ИД=%s, топик=%s", esuMessage.getKey(), esuMessage.getTopic()));
        try {
            EsuInput input = new EsuInput(esuMessage.getKey(), esuMessage.getOffset(), esuMessage.getPartition(),
                esuMessage.getTopic(), esuMessage.getBody(), LocalDateTime.now(), LocalDateTime.now());
            esuInputCRUDRepository.save(input);
        } catch (Throwable e) {
            String msg = String.format("Ошибка сохранения сообщения от ЕСУ ИД=%s, топик=%s", esuMessage.getKey(), esuMessage.getTopic());
            LOG.error(msg, e);
            throw e;
        }
    }
}
