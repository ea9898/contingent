package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.nsi.domain.area.NsiAddressFormingElement;
import moscow.ptnl.contingent.nsi.domain.area.NsiBuildingRegistry;
import moscow.ptnl.contingent.repository.BaseRepository;

import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class AddressesRepositoryImpl extends BaseRepository implements AddressesRepository {

    @Override
    public List<Addresses> findAddresses(long level, NsiBuildingRegistry buildingRegistry, NsiAddressFormingElement addressFormingElement) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Addresses> criteria = criteriaBuilder.createQuery(Addresses.class);
        Root<Addresses> address = criteria.from(Addresses.class);
        criteria.where(
                criteriaBuilder.and(
                        //TODO fix
//                        criteriaBuilder.equal(address.get(Addresses_.level.getName()), level),
//                        buildingRegistry == null ? address.get(Addresses_.buildingRegistry.getName()).isNull() :
//                                criteriaBuilder.equal(address.get(Addresses_.buildingRegistry.getName()), buildingRegistry),
//                        addressFormingElement == null ? address.get(Addresses_.addressFormingElement.getName()).isNull() :
//                                criteriaBuilder.equal(address.get(Addresses_.addressFormingElement.getName()), addressFormingElement)
                )
        );
        return entityManager.createQuery(criteria).getResultList();
    }
}
