package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.Addresses_;
import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.BuildingRegistry;
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
    public List<Addresses> findAddresses(long level, BuildingRegistry buildingRegistry, AddressFormingElement addressFormingElement) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Addresses> criteria = criteriaBuilder.createQuery(Addresses.class);
        Root<Addresses> address = criteria.from(Addresses.class);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(address.get(Addresses_.level.getName()), level),
                        buildingRegistry == null ? address.get(Addresses_.buildingRegistry.getName()).isNull() :
                                criteriaBuilder.equal(address.get(Addresses_.buildingRegistry.getName()), buildingRegistry),
                        addressFormingElement == null ? address.get(Addresses_.addressFormingElement.getName()).isNull() :
                                criteriaBuilder.equal(address.get(Addresses_.addressFormingElement.getName()), addressFormingElement)
                )
        );
        return entityManager.createQuery(criteria).getResultList();
    }
}
