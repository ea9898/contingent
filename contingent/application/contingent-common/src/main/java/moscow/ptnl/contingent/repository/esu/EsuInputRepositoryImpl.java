package moscow.ptnl.contingent.repository.esu;

import moscow.ptnl.contingent.domain.esu.EsuInput;
import moscow.ptnl.contingent.domain.esu.EsuInput_;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
@Transactional(propagation = Propagation.MANDATORY)
public class EsuInputRepositoryImpl extends BaseRepository implements EsuInputRepository {

    @Autowired
    EsuInputCRUDRepository esuInputCRUDRepository;

    @Override
    public List<EsuInput> findByTopic(String topic, String personalTopic) {
        Specification<EsuInput> specification =
                (root, criteriaQuery, criteriaBuilder) ->
                        criteriaBuilder.and(
                                        criteriaBuilder.or(
                                                criteriaBuilder.equal(root.get(EsuInput_.topic), topic),
                                                criteriaBuilder.equal(root.get(EsuInput_.topic), personalTopic)),
                                        criteriaBuilder.isNull(root.get(EsuInput_.status)));

        return esuInputCRUDRepository.findAll(specification);
    }

    @Override
    public List<EsuInput> findByEventId(String eventId) {
        Specification<EsuInput> specification =
                (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.equal(root.get(EsuInput_.eventId), eventId);

        return esuInputCRUDRepository.findAll(specification);
    }
}
