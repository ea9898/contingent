package moscow.ptnl.contingent.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.NsiAddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.NsiAddressFormingElement_;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class AddressFormingElementRepositoryImpl extends BaseRepository implements AddressFormingElementRepository {

    @Autowired
    AddressFormingElementCRUDRepository addressFormingElementCRUDRepository;

    private Specification<NsiAddressFormingElement> searchByIdSpec(long afeId) {
        return (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(NsiAddressFormingElement_.id.getName()), afeId);
    }

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
    public List<NsiAddressFormingElement> findAfeByIdAndLevel(long afeId, int level) {
        return addressFormingElementCRUDRepository.findAll(searchByIdSpec(afeId).and(searchByLevelSpec(level)));
    }

    @Override
    public NsiAddressFormingElement findAfeByGlobalId(Long globalId) {
        return addressFormingElementCRUDRepository.findOne(searchByGlobalIdSpec(globalId)).orElse(null);
    }

}
