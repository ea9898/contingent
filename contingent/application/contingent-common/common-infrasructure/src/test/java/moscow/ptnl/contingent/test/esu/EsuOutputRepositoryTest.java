package moscow.ptnl.contingent.test.esu;

import java.time.LocalDateTime;
import java.util.Optional;
import moscow.ptnl.contingent.domain.esu.EsuOutput;
import moscow.ptnl.contingent.domain.esu.EsuStatusType;
import moscow.ptnl.contingent.repository.esu.EsuOutputCRUDRepository;
import moscow.ptnl.contingent.repository.esu.EsuOutputRepository;
import moscow.ptnl.contingent.test.BaseTest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 * @author m.kachalov
 */
public class EsuOutputRepositoryTest extends BaseTest {
    
    @Autowired
    private EsuOutputCRUDRepository crudRepository;
    
    @Autowired
    private EsuOutputRepository repository;
    
    //@Test
    public void injectedTest(){
        Assertions.assertNotNull(crudRepository);
        Assertions.assertNotNull(repository);
    }
    
    @Test
    @Transactional
    public void updateTest() {
        EsuOutput e = new EsuOutput();
        e.setCreateDate(LocalDateTime.now());
        e.setSentTime(LocalDateTime.now());
        e.setTopic("topic");
        e.setMessage("-");
        e.setStatus(EsuStatusType.ТО_STATUS);
        e.setMethod("");
        e = crudRepository.save(e);
        Long id = e.getId();
        boolean updated = repository.updateMessage(id, "message", "method");
        Assertions.assertTrue(updated);
        Optional<EsuOutput> e1 = crudRepository.findById(id);
        Assertions.assertTrue(e1.isPresent());
        Assertions.assertEquals("message", e1.get().getMessage());
    }
    
}
