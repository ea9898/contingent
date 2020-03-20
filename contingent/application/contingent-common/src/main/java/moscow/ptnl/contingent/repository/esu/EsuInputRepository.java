package moscow.ptnl.contingent.repository.esu;

import moscow.ptnl.contingent.domain.esu.EsuInput;
import org.springframework.data.repository.NoRepositoryBean;
import java.util.List;

@NoRepositoryBean
public interface EsuInputRepository {

    List<EsuInput> findByTopic(String topic, String personalTopic);

    List<EsuInput> findByEventId(String eventId);
}
