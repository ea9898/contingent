package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.Addresses_;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress_;
import moscow.ptnl.contingent.domain.area.repository.AddressesRepository;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;
import jakarta.persistence.criteria.Predicate;
import org.springframework.util.StringUtils;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@Repository
@Transactional(propagation = Propagation.MANDATORY)
public class AddressesRepositoryImpl extends BaseRepository implements AddressesRepository {

    @Autowired
    private AddressesCRUDRepository addressesCRUDRepository;

    @Autowired
    private AreaAddressPagingAndSortingRepository areaAddressPagingAndSortingRepository;

    @Override
    public List<Addresses> findAddresses(List<Long> nsiGlobalIds, String aoLevel) {
        Specification<Addresses> specification = (Specification<Addresses>) (root, criteriaQuery, criteriaBuilder) ->
                StringUtils.hasLength(aoLevel) ? criteriaBuilder.and(
                        criteriaBuilder.in(root.get(Addresses_.globalId.getName())).value(nsiGlobalIds),
                        criteriaBuilder.equal(root.get(Addresses_.aoLevel), aoLevel)
                )
                : criteriaBuilder.in(root.get(Addresses_.globalId.getName())).value(nsiGlobalIds);
        return addressesCRUDRepository.findAll(specification);
    }

    @Override
    public List<Addresses> findAddresses(String areaOMKTECode, String regionOMKTECode, String aoLevel) {
        if (areaOMKTECode == null && regionOMKTECode == null) {
            throw new IllegalArgumentException("areaOMKTECode == null && regionOMKTECode == null");
        }
        Specification<Addresses> specification = (Specification<Addresses>) (root, criteriaQuery, criteriaBuilder) ->
            criteriaBuilder.and(
                    areaOMKTECode == null ? criteriaBuilder.conjunction() : criteriaBuilder.equal(root.get(Addresses_.areaCodeOmkTe), areaOMKTECode),
                    regionOMKTECode == null ? criteriaBuilder.conjunction() : criteriaBuilder.equal(root.get(Addresses_.regionTeCode), regionOMKTECode),
                    aoLevel == null ? criteriaBuilder.conjunction() : criteriaBuilder.equal(root.get(Addresses_.aoLevel), aoLevel)
            );
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

        return areaAddressPagingAndSortingRepository.findAll(specification)
                .stream().map(AreaAddress::getAddress).collect(Collectors.toList());
    }

    @Override
    public Set<Addresses> findActualAddresses(String streetCode, String planCode,
                                              String placeCode, String cityCode, String areaCode,
                                              List<String> areaOmkTeCodes, List<String> regionTeCodes, String aoLevel) {
        Specification<AreaAddress> specification = (Specification<AreaAddress>) (root, criteriaQuery, criteriaBuilder) -> {
            final Join<AreaAddress, Addresses> addressesJoin = root.join(AreaAddress_.address, JoinType.LEFT);
            return criteriaBuilder.and(
                    criteriaBuilder.or(
                            criteriaBuilder.greaterThanOrEqualTo(root.get(AreaAddress_.endDate), LocalDate.now()),
                            criteriaBuilder.isNull(root.get(AreaAddress_.endDate))
                    ),
                    aoLevel == null ? criteriaBuilder.conjunction() :
                            criteriaBuilder.and(
                                    criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel), aoLevel),
                    streetCode == null ? criteriaBuilder.conjunction() :
                            criteriaBuilder.and(
                                    criteriaBuilder.equal(addressesJoin.get(Addresses_.streetCode), streetCode)),
                    planCode == null ? criteriaBuilder.conjunction() :
                            criteriaBuilder.and(
                                    criteriaBuilder.equal(addressesJoin.get(Addresses_.planCode), planCode)),
                    placeCode == null ? criteriaBuilder.conjunction() :
                            criteriaBuilder.and(
                                    criteriaBuilder.equal(addressesJoin.get(Addresses_.placeCode), placeCode)),
                    cityCode == null ? criteriaBuilder.conjunction() :
                            criteriaBuilder.and(
                                    criteriaBuilder.equal(addressesJoin.get(Addresses_.cityCode), cityCode)),
                    areaCode == null ? criteriaBuilder.conjunction() :
                            criteriaBuilder.and(
                                    criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCode), areaCode)),
                    areaOmkTeCodes == null || areaOmkTeCodes.size() == 0 ? criteriaBuilder.conjunction() :
                            criteriaBuilder.and(
//                                    addressesJoin.get(Addresses_.areaCodeOmkTe).in(areaOmkTeCodes),
                                    //AREACODE_OMK_TE LIKE '%AREACODE_OMK_TE%' (если в адресе из входных параметров содержится несколько значений, разделенных ";", то данное условие проверяется для каждого значения)
                                    criteriaBuilder.or(areaOmkTeCodes.stream()
                                            .distinct()
                                            .map(c -> criteriaBuilder.like(addressesJoin.get(Addresses_.areaCodeOmkTe), "%" + c + "%"))
                                            .toArray(Predicate[]::new))),
                    regionTeCodes == null || regionTeCodes.size() == 0 ? criteriaBuilder.conjunction() :
                            criteriaBuilder.and(
//                                    addressesJoin.get(Addresses_.regionTeCode).in(regionTeCodes),
                                    //REGION_TE_CODE LIKE '%REGION_TE_CODE%' (если в адресе из входных параметров содержится несколько значений, разделенных ";", то данное условие проверяется для каждого значения)
                                    criteriaBuilder.or(regionTeCodes.stream()
                                            .distinct()
                                            .map(c -> criteriaBuilder.like(addressesJoin.get(Addresses_.regionTeCode), "%" + c + "%"))
                                            .toArray(Predicate[]::new))))
            );
        };
        return areaAddressPagingAndSortingRepository.findAll(specification)
                .stream().map(AreaAddress::getAddress).collect(Collectors.toSet());
    }

    @Override
    public List<Addresses> saveAll(List<Addresses> addresses) {
        return addressesCRUDRepository.saveAll(addresses);
    }

    @Override
    public Optional<Addresses> findAddressesByGlobalIdNsi(Long globalIdNsi) {
        Specification<Addresses> specification = (Specification<Addresses>) (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(Addresses_.globalId), globalIdNsi);

        return addressesCRUDRepository.findOne(specification);
    }
}
