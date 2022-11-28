package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress_;
import moscow.ptnl.contingent.domain.area.entity.Area_;
import moscow.ptnl.contingent.domain.area.entity.MoAddress_;
import moscow.ptnl.contingent.domain.area.model.area.MoMuPair;
import moscow.ptnl.contingent.domain.area.repository.AreaAddressRepository;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import moscow.ptnl.contingent.domain.area.entity.Addresses_;
import moscow.ptnl.contingent.nsi.domain.area.AreaType_;
import moscow.ptnl.contingent.repository.CommonSpecification;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

@Repository
@Transactional(propagation = Propagation.MANDATORY)
public class AreaAddressRepositoryImpl extends BaseRepository implements AreaAddressRepository {

    @Autowired
    AreaAddressPagingAndSortingRepository areaAddressPagingAndSortingRepository;

    // Спека поиска актуальных территорий обслуживания
    private Specification<AreaAddress> activeAreaAddressesSpec() {
        return (Specification<AreaAddress>) (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.or(
                        criteriaBuilder.greaterThanOrEqualTo(root.get(AreaAddress_.endDate.getName()), LocalDate.now()),
                        root.get(AreaAddress_.endDate.getName()).isNull()
                );
    }

    // Спека поиска актуальных территорий обслуживания
    private Specification<AreaAddress> actualAreaAddressesSpec() {
        return (root, criteriaQuery, builder) -> {
            LocalDate now = LocalDate.now();

            return builder.and(
                    builder.or(
                            builder.lessThanOrEqualTo(root.get(MoAddress_.startDate.getName()), now),
                            root.get(MoAddress_.startDate.getName()).isNull()
                    ),
                    builder.or(
                            builder.greaterThan(root.get(MoAddress_.endDate.getName()), now),
                            root.get(MoAddress_.endDate.getName()).isNull()
                    )
            );
        };
    }

    // Спека поиска территорий обслуживания по ID
    private Specification<AreaAddress> findMoAddressesByIdsSpec(List<Long> moAddressIds) {
        return (root, criteriaQuery, builder) -> {
            root.fetch(AreaAddress_.address, JoinType.INNER);
            root.fetch(AreaAddress_.moAddress, JoinType.INNER);
            root.fetch(AreaAddress_.area, JoinType.INNER);
            return builder.in(root.get(AreaAddress_.moAddress.getName()).get(MoAddress_.id.getName())).value(moAddressIds);
        };
    }

    private Specification<AreaAddress> findAreaAddressesByIdsSpec(List<Long> areaAddressIds) {
        return CommonSpecification.in(AreaAddress_.id, areaAddressIds);
        //return (Specification<AreaAddress>) (root, criteriaQuery, criteriaBuilder) ->
        //        root.get(AreaAddress_.id.getName()).in(areaAddressIds);                
    }

    @Override
    public List<AreaAddress> getActiveAreaAddresses(long moId, long areaTypeCode) {
        Specification<AreaAddress> specification = (Specification<AreaAddress>) (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                        criteriaBuilder.equal(root.get(AreaAddress_.area.getName()).get(Area_.moId.getName()), moId),
                        criteriaBuilder.equal(root.get(AreaAddress_.area.getName()).get(Area_.areaType.getName()).get(AreaType_.code.getName()), areaTypeCode)
                );

        specification = specification.and(activeAreaAddressesSpec());
        return areaAddressPagingAndSortingRepository.findAll(specification);
    }

    @Override
    public List<AreaAddress> getActiveAreaAddressesV3(Long moId, AreaType areaTypeCode, Addresses globalIdNsi) {
        Specification<AreaAddress> specification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<AreaAddress, Addresses> addressesJoin = root.join(AreaAddress_.address, JoinType.INNER);

            List<Predicate> predicates = new ArrayList<>();
            addEndDateMoIdAreaTypePredicates(root, criteriaBuilder, predicates, moId, areaTypeCode);
            predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.globalId.getName()), globalIdNsi.getGlobalId()));
            return criteriaBuilder.and(predicates.toArray(new Predicate[0]));
        };

        return areaAddressPagingAndSortingRepository.findAll(specification);
    }

    private Predicate getPredicateAreaAddressLevel8AoLevel7(Addresses addresses, Join<AreaAddress, Addresses> addressesJoin, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(addressesJoin.get(Addresses_.streetCode.getName()).isNotNull());
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.streetCode.getName()), addresses.getStreetCode()));
        predicates.add(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()).isNotNull());
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()), addresses.getAreaCodeOmkTe()));
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "7"));
        if (addresses.getPlanCode() != null) {
            predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.planCode.getName()), addresses.getPlanCode()));
        } else {
            predicates.add(addressesJoin.get(Addresses_.planCode.getName()).isNull());
        }
        if (addresses.getPlaceCode() != null) {
            predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.placeCode.getName()), addresses.getPlaceCode()));
        } else {
            predicates.add(addressesJoin.get(Addresses_.placeCode.getName()).isNull());
        }
        if (addresses.getCityCode() != null) {
            predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.cityCode.getName()), addresses.getCityCode()));
        } else {
            predicates.add(addressesJoin.get(Addresses_.cityCode.getName()).isNull());
        }

        return criteriaBuilder.and(predicates.toArray(new Predicate[]{}));
    }

    private Predicate getPredicateAreaAddressCheckedAoLevel65(Addresses addresses, Join<AreaAddress, Addresses> addressesJoin, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()).isNotNull());
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()), addresses.getAreaCodeOmkTe()));
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "65"));
        if (addresses.getPlanCode() != null) {
            predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.planCode.getName()), addresses.getPlanCode()));
        } else {
            predicates.add(addressesJoin.get(Addresses_.planCode.getName()).isNull());
        }
        if (addresses.getPlaceCode() != null) {
            predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.placeCode.getName()), addresses.getPlaceCode()));
        } else {
            predicates.add(addressesJoin.get(Addresses_.placeCode.getName()).isNull());
        }
        if (addresses.getCityCode() != null) {
            predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.cityCode.getName()), addresses.getCityCode()));
        } else {
            predicates.add(addressesJoin.get(Addresses_.cityCode.getName()).isNull());
        }

        return criteriaBuilder.and(predicates.toArray(new Predicate[]{}));
    }

    private Predicate getPredicateAreaAddressCheckedAoLevel6(Addresses addresses, Join<AreaAddress, Addresses> addressesJoin, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()).isNotNull());
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()), addresses.getAreaCodeOmkTe()));
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "6"));
        if (addresses.getPlaceCode() != null) {
            predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.placeCode.getName()), addresses.getPlaceCode()));
        } else {
            predicates.add(addressesJoin.get(Addresses_.placeCode.getName()).isNull());
        }
        if (addresses.getCityCode() != null) {
            predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.cityCode.getName()), addresses.getCityCode()));
        } else {
            predicates.add(addressesJoin.get(Addresses_.cityCode.getName()).isNull());
        }

        return criteriaBuilder.and(predicates.toArray(new Predicate[]{}));
    }

    private Predicate getPredicateAreaAddressCheckedAoLevel4(Addresses addresses, Join<AreaAddress, Addresses> addressesJoin, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()).isNotNull());
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()), addresses.getAreaCodeOmkTe()));
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "4"));
        if (addresses.getCityCode() != null) {
            predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.cityCode.getName()), addresses.getCityCode()));
        } else {
            predicates.add(addressesJoin.get(Addresses_.cityCode.getName()).isNull());
        }

        return criteriaBuilder.and(predicates.toArray(new Predicate[]{}));
    }

    private void addEndDateMoIdAreaTypePredicates(Root<AreaAddress> root, CriteriaBuilder cb, List<Predicate> predicates,
                                                  Long moId, AreaType areaTypeCode) {
        Join<AreaAddress, Area> areaJoin = root.join(AreaAddress_.area, JoinType.INNER);
        predicates.add(root.get(AreaAddress_.endDate.getName()).isNull());

        if (areaTypeCode != null) {
            predicates.add(cb.equal(areaJoin.get(Area_.areaType.getName()), areaTypeCode.getCode()));
        }
        if (moId != null) {
            predicates.add(cb.equal(areaJoin.get(Area_.moId.getName()), moId));
        }
    }

    @Override
    public List<AreaAddress> getActiveAreaAddressesLevel8(Long moId, AreaType areaTypeCode, Addresses addresses) {
        Specification<AreaAddress> specification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<AreaAddress, Addresses> addressesJoin = root.join(AreaAddress_.address, JoinType.INNER);

            List<Predicate> predicates = new ArrayList<>();
            addEndDateMoIdAreaTypePredicates(root, criteriaBuilder, predicates, moId, areaTypeCode);

            predicates.add(criteriaBuilder.or(
                    getPredicateAreaAddressLevel8AoLevel7(addresses, addressesJoin, criteriaBuilder),
                    getPredicateAreaAddressCheckedAoLevel65(addresses, addressesJoin, criteriaBuilder),
                    getPredicateAreaAddressCheckedAoLevel6(addresses, addressesJoin, criteriaBuilder),
                    getPredicateAreaAddressCheckedAoLevel4(addresses, addressesJoin, criteriaBuilder),
                    criteriaBuilder.or(
                            criteriaBuilder.and(
                                    addressesJoin.get(Addresses_.areaCodeOmkTe.getName()).isNotNull(),
                                    criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()), addresses.getAreaCodeOmkTe()),
                                    criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "25")
                            )
                    ),
                    criteriaBuilder.or(
                            criteriaBuilder.and(
                                    addressesJoin.get(Addresses_.regionTeCode.getName()).isNotNull(),
                                    criteriaBuilder.equal(addressesJoin.get(Addresses_.regionTeCode.getName()), addresses.getRegionTeCode()),
                                    criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "2")
                            )
                    )
            ));
            return criteriaBuilder.and(predicates.toArray(new Predicate[0]));
        };

        return areaAddressPagingAndSortingRepository.findAll(specification);
    }

    private Predicate getPredicateAreaAddressLevel7AoLevel7(Addresses addresses, Join<AreaAddress, Addresses> addressesJoin, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(addressesJoin.get(Addresses_.streetCode.getName()).isNotNull());
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.streetCode.getName()), addresses.getStreetCode()));
        predicates.add(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()).isNotNull());
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()), addresses.getAreaCodeOmkTe()));
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "7"));
        if (addresses.getPlanCode() != null) {
            predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.planCode.getName()), addresses.getPlanCode()));
        } else {
            predicates.add(addressesJoin.get(Addresses_.planCode.getName()).isNull());
        }
        if (addresses.getPlaceCode() != null) {
            predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.placeCode.getName()), addresses.getPlaceCode()));
        } else {
            predicates.add(addressesJoin.get(Addresses_.placeCode.getName()).isNull());
        }
        if (addresses.getCityCode() != null) {
            predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.cityCode.getName()), addresses.getCityCode()));
        } else {
            predicates.add(addressesJoin.get(Addresses_.cityCode.getName()).isNull());
        }

        return criteriaBuilder.and(predicates.toArray(new Predicate[]{}));
    }

    @Override
    public List<AreaAddress> getActiveAreaAddressesLevel7(Long moId, AreaType areaTypeCode, Addresses addresses) {
        Specification<AreaAddress> specification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<AreaAddress, Addresses> addressesJoin = root.join(AreaAddress_.address, JoinType.INNER);

            List<Predicate> predicates = new ArrayList<>();
            addEndDateMoIdAreaTypePredicates(root, criteriaBuilder, predicates, moId, areaTypeCode);

            predicates.add(criteriaBuilder.or(
                    getPredicateAreaAddressLevel7AoLevel7(addresses, addressesJoin, criteriaBuilder),
                    getPredicateAreaAddressCheckedAoLevel65(addresses, addressesJoin, criteriaBuilder),
                    getPredicateAreaAddressCheckedAoLevel6(addresses, addressesJoin, criteriaBuilder),
                    getPredicateAreaAddressCheckedAoLevel4(addresses, addressesJoin, criteriaBuilder),
                    criteriaBuilder.and(
                            addressesJoin.get(Addresses_.areaCodeOmkTe.getName()).isNotNull(),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()), addresses.getAreaCodeOmkTe()),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "25")
                    ),
                    criteriaBuilder.and(
                            addressesJoin.get(Addresses_.regionTeCode.getName()).isNotNull(),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.regionTeCode.getName()), addresses.getRegionTeCode()),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "2")
                    )
            ));
            return criteriaBuilder.and(predicates.toArray(new Predicate[0]));
        };

        return areaAddressPagingAndSortingRepository.findAll(specification);
    }

    @Override
    public List<AreaAddress> getActiveAreaAddressesLevel65(Long moId, AreaType areaTypeCode, Addresses addresses) {
        Specification<AreaAddress> specification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<AreaAddress, Addresses> addressesJoin = root.join(AreaAddress_.address, JoinType.INNER);

            List<Predicate> predicates = new ArrayList<>();
            addEndDateMoIdAreaTypePredicates(root, criteriaBuilder, predicates, moId, areaTypeCode);

            predicates.add(criteriaBuilder.or(
                    getPredicateAreaAddressCheckedAoLevel65(addresses, addressesJoin, criteriaBuilder),
                    getPredicateAreaAddressCheckedAoLevel6(addresses, addressesJoin, criteriaBuilder),
                    getPredicateAreaAddressCheckedAoLevel4(addresses, addressesJoin, criteriaBuilder),
                    criteriaBuilder.and(
                            addressesJoin.get(Addresses_.areaCodeOmkTe.getName()).isNotNull(),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()), addresses.getAreaCodeOmkTe()),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "25")
                    ),
                    criteriaBuilder.and(
                            addressesJoin.get(Addresses_.regionTeCode.getName()).isNotNull(),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.regionTeCode.getName()), addresses.getRegionTeCode()),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "2")
                    )
            ));
            return criteriaBuilder.and(predicates.toArray(new Predicate[0]));
        };

        return areaAddressPagingAndSortingRepository.findAll(specification);
    }

    @Override
    public List<AreaAddress> getActiveAreaAddressesLevel6(Long moId, AreaType areaTypeCode, Addresses addresses) {
        Specification<AreaAddress> specification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<AreaAddress, Addresses> addressesJoin = root.join(AreaAddress_.address, JoinType.INNER);

            List<Predicate> predicates = new ArrayList<>();
            addEndDateMoIdAreaTypePredicates(root, criteriaBuilder, predicates, moId, areaTypeCode);

            predicates.add(criteriaBuilder.or(
                    getPredicateAreaAddressCheckedAoLevel6(addresses, addressesJoin, criteriaBuilder),
                    getPredicateAreaAddressCheckedAoLevel4(addresses, addressesJoin, criteriaBuilder),
                    criteriaBuilder.and(
                            addressesJoin.get(Addresses_.areaCodeOmkTe.getName()).isNotNull(),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()), addresses.getAreaCodeOmkTe()),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "25")
                    ),
                    criteriaBuilder.and(
                            addressesJoin.get(Addresses_.regionTeCode.getName()).isNotNull(),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.regionTeCode.getName()), addresses.getRegionTeCode()),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "2")
                    )
            ));
            return criteriaBuilder.and(predicates.toArray(new Predicate[0]));
        };

        return areaAddressPagingAndSortingRepository.findAll(specification);
    }

    @Override
    public List<AreaAddress> getActiveAreaAddressesLevel4(Long moId, AreaType areaTypeCode, Addresses addresses) {
        Specification<AreaAddress> specification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<AreaAddress, Addresses> addressesJoin = root.join(AreaAddress_.address, JoinType.INNER);

            List<Predicate> predicates = new ArrayList<>();
            addEndDateMoIdAreaTypePredicates(root, criteriaBuilder, predicates, moId, areaTypeCode);

            predicates.add(criteriaBuilder.or(
                    getPredicateAreaAddressCheckedAoLevel4(addresses, addressesJoin, criteriaBuilder),
                    criteriaBuilder.and(
                            addressesJoin.get(Addresses_.areaCodeOmkTe.getName()).isNotNull(),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()), addresses.getAreaCodeOmkTe()),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "25")
                    ),
                    criteriaBuilder.and(
                            addressesJoin.get(Addresses_.regionTeCode.getName()).isNotNull(),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.regionTeCode.getName()), addresses.getRegionTeCode()),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "2")
                    )
            ));
            return criteriaBuilder.and(predicates.toArray(new Predicate[0]));
        };

        return areaAddressPagingAndSortingRepository.findAll(specification);
    }

    @Override
    public List<AreaAddress> getActiveAreaAddressesLevel25(Long moId, AreaType areaTypeCode, Addresses addresses) {
        Specification<AreaAddress> specification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<AreaAddress, Addresses> addressesJoin = root.join(AreaAddress_.address, JoinType.INNER);

            List<Predicate> predicates = new ArrayList<>();
            addEndDateMoIdAreaTypePredicates(root, criteriaBuilder, predicates, moId, areaTypeCode);

            predicates.add(criteriaBuilder.or(
                    criteriaBuilder.and(
                            addressesJoin.get(Addresses_.areaCodeOmkTe.getName()).isNotNull(),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()), addresses.getAreaCodeOmkTe()),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "25")
                    ),
                    criteriaBuilder.and(
                            addressesJoin.get(Addresses_.regionTeCode.getName()).isNotNull(),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.regionTeCode.getName()), addresses.getRegionTeCode()),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "2")
                    )
            ));
            return criteriaBuilder.and(predicates.toArray(new Predicate[0]));
        };

        return areaAddressPagingAndSortingRepository.findAll(specification);
    }

    @Override
    public List<AreaAddress> getActiveAreaAddressesLevel2(Long moId, AreaType areaTypeCode, Addresses addresses) {
        Specification<AreaAddress> specification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<AreaAddress, Addresses> addressesJoin = root.join(AreaAddress_.address, JoinType.INNER);

            List<Predicate> predicates = new ArrayList<>();
            addEndDateMoIdAreaTypePredicates(root, criteriaBuilder, predicates, moId, areaTypeCode);

            predicates.add(criteriaBuilder.or(
                    criteriaBuilder.and(
                            addressesJoin.get(Addresses_.regionTeCode.getName()).isNotNull(),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.regionTeCode.getName()), addresses.getRegionTeCode()),
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "2")
                    )
            ));
            return criteriaBuilder.and(predicates.toArray(new Predicate[0]));
        };

        return areaAddressPagingAndSortingRepository.findAll(specification);
    }

    @Override
    public List<AreaAddress> findAreaAddresses(List<Long> moAddressIds) {
        return areaAddressPagingAndSortingRepository.findAll(findMoAddressesByIdsSpec(moAddressIds));
    }

    @Override
    public List<AreaAddress> findActualAreaAddresses(List<Long> moAddressIds) {
        return areaAddressPagingAndSortingRepository.findAll(findMoAddressesByIdsSpec(moAddressIds).and(actualAreaAddressesSpec()));
    }

    @Override
    public List<AreaAddress> findAreaAddressesActual(List<Long> areaAddressIds) {
        return areaAddressPagingAndSortingRepository.findAll(findAreaAddressesByIdsSpec(areaAddressIds).and(activeAreaAddressesSpec()));
    }

    @Override
    public Page<AreaAddress> findAreaAddressesByAreaId(Long moId, List<Long> areaIds, Pageable paging) {
        Specification<AreaAddress> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                        criteriaBuilder.in(root.get(AreaAddress_.area.getName()).get(Area_.moId.getName())).value(moId), //root.get(AreaAddress_.area.getName()).get(Area_.moId.getName()).in(moId),
                        criteriaBuilder.in(root.get(AreaAddress_.area.getName()).get(Area_.id.getName())).value(areaIds) //root.get(AreaAddress_.area.getName()).get(Area_.id.getName()).in(areaIds)
                );
        specification = specification.and(activeAreaAddressesSpec());


        if (paging == null) {
            return new PageImpl<>(areaAddressPagingAndSortingRepository.findAll(specification, Sort.by(AreaAddress_.area.getName(), AreaAddress_.id.getName()).ascending()));
        }
        return areaAddressPagingAndSortingRepository.findAll(specification,
                PageRequest.of(paging.getPageNumber(), paging.getPageSize(), Sort.by(AreaAddress_.area.getName(), AreaAddress_.id.getName()).ascending()));
    }

    @Override
    public List<AreaAddress> findAreaAddressByAddressIds(List<Long> addressIds) {
        Specification<AreaAddress> specification = (root, criteriaQuery, cb) ->
                cb.and(
                        cb.in(root.get(AreaAddress_.address.getName()).get(Addresses_.id.getName())).value(addressIds),
                        cb.isNull(root.get(AreaAddress_.endDate))
//                        cb.or(
//                                cb.greaterThanOrEqualTo(root.get(AreaAddress_.endDate), LocalDate.now()),
//                                cb.isNull(root.get(AreaAddress_.endDate))
//                        )
                );

        return areaAddressPagingAndSortingRepository.findAll(specification);
    }

    @Override
    public void delete(AreaAddress areaAddress) {
        areaAddressPagingAndSortingRepository.delete(areaAddress);
    }

    @Override
    public List<AreaAddress> saveAll(List<AreaAddress> addresses) {
        return areaAddressPagingAndSortingRepository.saveAll(addresses);
    }

    @Override
    public AreaAddress save(AreaAddress areaAddress) {
        return areaAddressPagingAndSortingRepository.save(areaAddress);
    }

    @Override
    public Page<MoMuPair> findMoMuList(List<Long> areaTypeCodes, String areaOMKTECode, String regionOMKTECode,
                                       String regionTeCode, String aoLevelRegionTe, LocalDate endDate, PageRequest paging) {
        return areaAddressPagingAndSortingRepository.findMoMuList(areaTypeCodes, areaOMKTECode, regionOMKTECode,
                regionTeCode, aoLevelRegionTe, endDate, paging);
    }

    @Override
    public Page<MoMuPair> findMoMuList(List<Long> areaTypeCodes, List<Long> addressIds, LocalDate endDate, PageRequest paging) {
        return areaAddressPagingAndSortingRepository.findMoMuList(areaTypeCodes, addressIds, endDate, paging);
    }
}
