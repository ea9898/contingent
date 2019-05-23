package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement_;
import moscow.ptnl.contingent.area.repository.BaseRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class AddressFormingElementRepositoryImpl extends BaseRepository implements AddressFormingElementRepository {

    @Override
    public List<AddressFormingElement> getAddressFormingElements(long globalId, int level) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<AddressFormingElement> criteria = criteriaBuilder.createQuery(AddressFormingElement.class);
        Root<AddressFormingElement> template = criteria.from(AddressFormingElement.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(template.get(AddressFormingElement_.globalId.getName()), globalId),
                        criteriaBuilder.equal(template.get(AddressFormingElement_.aoLevel.getName()), String.valueOf(level))
                )
        );
        return entityManager.createQuery(criteria).getResultList();
    }



}
