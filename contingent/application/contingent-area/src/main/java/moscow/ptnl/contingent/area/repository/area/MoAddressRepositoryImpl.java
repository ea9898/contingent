package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.area.MoAddress_;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypes_;
import moscow.ptnl.contingent.area.repository.BaseRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.time.LocalDate;
import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class MoAddressRepositoryImpl extends BaseRepository implements MoAddressRepository {

    @Override
    public List<MoAddress> getActiveMoAddresses(long moId, long areaTypeCode) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<MoAddress> criteria = criteriaBuilder.createQuery(MoAddress.class);
        Root<MoAddress> address = criteria.from(MoAddress.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(address.get(MoAddress_.moId.getName()), moId),
                        criteriaBuilder.equal(address.get(MoAddress_.areaType.getName()).get(AreaTypes_.code.getName()), areaTypeCode),
                        criteriaBuilder.or(
                                criteriaBuilder.greaterThanOrEqualTo(address.get(MoAddress_.endDate.getName()), LocalDate.now()),
                                address.get(MoAddress_.endDate.getName()).isNull()
                        )
                )
        );
        return entityManager.createQuery(criteria).getResultList();
    }
}
