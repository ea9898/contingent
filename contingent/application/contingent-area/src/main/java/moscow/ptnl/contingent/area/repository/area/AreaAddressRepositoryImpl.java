package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.area.AreaAddress_;
import moscow.ptnl.contingent.area.entity.area.Area_;
import moscow.ptnl.contingent.area.entity.nsi.AreaType_;
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
public class AreaAddressRepositoryImpl extends BaseRepository implements AreaAddressRepository {

    @Override
    public List<AreaAddress> getActiveAreaAddresses(long moId, long areaTypeCode) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<AreaAddress> criteria = criteriaBuilder.createQuery(AreaAddress.class);
        Root<AreaAddress> address = criteria.from(AreaAddress.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(address.get(AreaAddress_.area.getName()).get(Area_.moId.getName()), moId),
                        criteriaBuilder.equal(address.get(AreaAddress_.area.getName()).get(Area_.areaType.getName()).get(AreaType_.code.getName()), areaTypeCode),
                        criteriaBuilder.or(
                                criteriaBuilder.greaterThanOrEqualTo(address.get(AreaAddress_.endDate.getName()), LocalDate.now()),
                                address.get(AreaAddress_.endDate.getName()).isNull()
                        )
                )
        );
        return entityManager.createQuery(criteria).getResultList();
    }
}
