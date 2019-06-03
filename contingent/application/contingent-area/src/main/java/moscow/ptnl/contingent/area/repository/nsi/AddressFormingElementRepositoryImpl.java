package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement_;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class AddressFormingElementRepositoryImpl extends BaseRepository implements AddressFormingElementRepository {

    @Autowired
    AddressFormingElementCRUDRepository addressFormingElementCRUDRepository;

    @Override
    public List<AddressFormingElement> getAddressFormingElements(long globalId, int level) {
        Specification<AddressFormingElement> addressFormingElementSpecification =
                (root, criteriaQuery, criteriaBuilder) ->
                    criteriaBuilder.and(
                        criteriaBuilder.equal(root.get(AddressFormingElement_.globalId.getName()), globalId),
                        criteriaBuilder.equal(root.get(AddressFormingElement_.aoLevel.getName()), String.valueOf(level))
                );
        return addressFormingElementCRUDRepository.findAll(addressFormingElementSpecification);
    }

    @Override
    public List<AddressFormingElement> findAfeByIdAndLevel(long afeId, int level) {
        Specification<AddressFormingElement> addressFormingElementSpecification =
            (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                    criteriaBuilder.equal(root.get(AddressFormingElement_.id.getName()), afeId),
                    criteriaBuilder.equal(root.get(AddressFormingElement_.aoLevel.getName()), level)
                );
        return addressFormingElementCRUDRepository.findAll(addressFormingElementSpecification);
    }

    @Override
    public AddressFormingElement findAfeByGlobalId(Long globalId) {
        Specification<AddressFormingElement> addressFormingElementSpecification =
            (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                    criteriaBuilder.equal(root.get(AddressFormingElement_.globalId.getName()), globalId));
        return addressFormingElementCRUDRepository.findOne(addressFormingElementSpecification).orElse(null);
    }

}
