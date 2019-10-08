package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.Addresses_;
import moscow.ptnl.contingent.nsi.domain.area.NsiAddressFormingElement;
import moscow.ptnl.contingent.nsi.domain.area.NsiBuildingRegistry;
import moscow.ptnl.contingent.repository.BaseRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
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

    @Autowired
    AddressesCRUDRepository addressesCRUDRepository;

    @Override
    public List<Addresses> findAddresses(List<Long> addressIds, List<Long> nsiGlobalIds) {
        Specification<Addresses> specification = (Specification<Addresses>) (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                        root.get(Addresses_.id).in(addressIds),
                        root.get(Addresses_.globalId).in(nsiGlobalIds));

        return addressesCRUDRepository.findAll(specification);
    }

    @Override
    public List<Addresses> findAddresses(List<Long> nsiGlobalIds) {
        Specification<Addresses> specification = (Specification<Addresses>) (root, criteriaQuery, criteriaBuilder) ->
                root.get(Addresses_.globalId).in(nsiGlobalIds);

        return addressesCRUDRepository.findAll(specification);
    }
}
