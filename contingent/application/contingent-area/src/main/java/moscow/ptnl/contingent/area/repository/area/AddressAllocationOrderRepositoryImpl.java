package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrder;
import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrder_;
import moscow.ptnl.contingent.area.repository.BaseRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.Date;
import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class AddressAllocationOrderRepositoryImpl extends BaseRepository implements AddressAllocationOrderRepository {

    @Override
    public List<AddressAllocationOrder> findAddressAllocationOrders(String number, Date date, String ouz, String name, Boolean archived) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<AddressAllocationOrder> criteria = criteriaBuilder.createQuery(AddressAllocationOrder.class);
        Root<AddressAllocationOrder> profile = criteria.from(AddressAllocationOrder.class);
        criteria.where(
                criteriaBuilder.and(
                        number == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(profile.get(AddressAllocationOrder_.number.getName()), number ),
                        date == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(profile.get(AddressAllocationOrder_.date.getName()), date),
                        ouz == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(profile.get(AddressAllocationOrder_.ouz.getName()), ouz),
                        name == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(profile.get(AddressAllocationOrder_.name.getName()), name),
                        archived == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(profile.get(AddressAllocationOrder_.archived.getName()), archived)
                )
        );
        return entityManager.createQuery(criteria).getResultList();
    }
}
