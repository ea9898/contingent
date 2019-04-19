package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrder;
import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrder_;
import moscow.ptnl.contingent.area.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.time.LocalDate;
import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class AddressAllocationOrderRepositoryImpl extends BaseRepository implements AddressAllocationOrderRepository {

    @Autowired
    private AddressAllocationOrderPagingAndSortingRepository addressAllocationOrderPagingAndSortingRepository;

    @Override
    public List<AddressAllocationOrder> findAddressAllocationOrders(String number, LocalDate date, String ouz, String name, Boolean archived) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<AddressAllocationOrder> criteria = criteriaBuilder.createQuery(AddressAllocationOrder.class);
        Root<AddressAllocationOrder> order = criteria.from(AddressAllocationOrder.class);
        criteria.where(
                criteriaBuilder.and(
                        number == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(order.get(AddressAllocationOrder_.number.getName()), number ),
                        date == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(order.get(AddressAllocationOrder_.date.getName()), date),
                        ouz == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(order.get(AddressAllocationOrder_.ouz.getName()), ouz),
                        name == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(order.get(AddressAllocationOrder_.name.getName()), name),
                        archived == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(order.get(AddressAllocationOrder_.archived.getName()), archived)
                )
        );
        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public Page<AddressAllocationOrder> findAddressAllocationOrdersOverlapped(Long id, String number, LocalDate date, String name, Pageable paging) {
        return addressAllocationOrderPagingAndSortingRepository.findAll((Specification<AddressAllocationOrder>) (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                        id == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(AddressAllocationOrder_.id.getName()), id),
                        number == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.like(
                                        criteriaBuilder.lower(root.get(AddressAllocationOrder_.number.getName())), "%" + number.toLowerCase() + "%"),
                        date == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(AddressAllocationOrder_.date.getName()), date),
                        name == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.like(
                                        criteriaBuilder.lower(root.get(AddressAllocationOrder_.name.getName())), "%" + name.toLowerCase() + "%"),
                        criteriaBuilder.equal(root.get(AddressAllocationOrder_.archived.getName()), false)
                ), paging);
    }
}
