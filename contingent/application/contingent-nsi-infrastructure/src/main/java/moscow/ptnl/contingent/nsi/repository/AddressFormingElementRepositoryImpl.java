package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import moscow.ptnl.contingent.nsi.domain.area.NsiAddressFormingElement;
import moscow.ptnl.contingent.nsi.domain.area.NsiAddressFormingElement_;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class AddressFormingElementRepositoryImpl extends BaseRepository implements AddressFormingElementRepository {

    @Autowired
    AddressFormingElementCRUDRepository addressFormingElementCRUDRepository;

    private Specification<NsiAddressFormingElement> searchByGlobalIdSpec(long globalId) {
        return (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(NsiAddressFormingElement_.globalId.getName()), globalId);
    }

    private Specification<NsiAddressFormingElement> searchByLevelSpec(int level) {
        return (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(NsiAddressFormingElement_.aoLevel.getName()), String.valueOf(level));
    }

    @Override
    public List<NsiAddressFormingElement> getAddressFormingElements(long globalId, int level) {
        return addressFormingElementCRUDRepository.findAll(searchByGlobalIdSpec(globalId).and(searchByLevelSpec(level)));
    }

    @Override
    public NsiAddressFormingElement findAfeByGlobalId(Long globalId) {
        return addressFormingElementCRUDRepository.findOne(searchByGlobalIdSpec(globalId)).orElse(null);
    }

}
