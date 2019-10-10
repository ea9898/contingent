package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.Addresses_;
import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.area.AreaAddress_;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import java.time.LocalDate;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Repository
@Transactional(propagation = Propagation.MANDATORY)
public class AddressesRepositoryImpl extends BaseRepository implements AddressesRepository {

    @Autowired
    AddressesCRUDRepository addressesCRUDRepository;

    @Autowired
    AreaAddressCRUDRepository areaAddressCRUDRepository;

    @Override
    public List<Addresses> findAddresses(List<Long> nsiGlobalIds) {
        Specification<Addresses> specification = (Specification<Addresses>) (root, criteriaQuery, criteriaBuilder) ->
                root.get(Addresses_.globalId).in(nsiGlobalIds);

        return addressesCRUDRepository.findAll(specification);
    }

    @Override
    public List<Addresses> findActualAddresses(List<Long> nsiGlobalIds) {
        Specification<AreaAddress> specification = (Specification<AreaAddress>) (root, criteriaQuery, criteriaBuilder) -> {
            final Join<AreaAddress, Addresses> addressesJoin = root.join(AreaAddress_.address, JoinType.LEFT);
            return criteriaBuilder.and(
                    criteriaBuilder.or(
                            criteriaBuilder.greaterThanOrEqualTo(root.get(AreaAddress_.endDate), LocalDate.now()),
                            criteriaBuilder.isNull(root.get(AreaAddress_.endDate))
                    ),
                    addressesJoin.get(Addresses_.globalId).in(nsiGlobalIds));
        };

        return areaAddressCRUDRepository.findAll(specification)
                .stream().map(AreaAddress::getAddress).collect(Collectors.toList());
    }

    @Override
    public Set<Addresses> findActualAddresses(List<Long> nsiGlobalIds, String streetCode, String planCode,
                                              String placeCode, String cityCode, String areaCode,
                                              List<String> areaOmkTeCodes, List<String> regionTeCodes) {
        Specification<AreaAddress> specification = (Specification<AreaAddress>) (root, criteriaQuery, criteriaBuilder) -> {
            final Join<AreaAddress, Addresses> addressesJoin = root.join(AreaAddress_.address, JoinType.LEFT);
            return criteriaBuilder.and(
                    criteriaBuilder.or(
                            criteriaBuilder.greaterThanOrEqualTo(root.get(AreaAddress_.endDate), LocalDate.now()),
                            criteriaBuilder.isNull(root.get(AreaAddress_.endDate))
                    ),
                    addressesJoin.get(Addresses_.globalId).in(nsiGlobalIds),
                    streetCode == null ? criteriaBuilder.conjunction() :
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.streetCode), streetCode),
                    planCode == null ? criteriaBuilder.conjunction() :
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.planCode), planCode),
                    placeCode == null ? criteriaBuilder.conjunction() :
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.placeCode), placeCode),
                    cityCode == null ? criteriaBuilder.conjunction() :
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.cityCode), cityCode),
                    areaCode == null ? criteriaBuilder.conjunction() :
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCode), areaCode),
                    areaOmkTeCodes == null || areaOmkTeCodes.size() == 0 ? criteriaBuilder.conjunction() :
                            addressesJoin.get(Addresses_.areaCodeOmkTe).in(areaOmkTeCodes),
                    regionTeCodes == null || regionTeCodes.size() == 0 ? criteriaBuilder.conjunction() :
                            addressesJoin.get(Addresses_.regionTeCode).in(regionTeCodes));
        };

        return areaAddressCRUDRepository.findAll(specification)
                .stream().map(AreaAddress::getAddress).collect(Collectors.toSet());
    }
}
