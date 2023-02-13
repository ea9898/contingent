package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.PageImplCustom;
import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders_;
import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.Addresses_;
import moscow.ptnl.contingent.nsi.domain.area.AreaType_;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.domain.area.entity.MoAddress_;
import moscow.ptnl.contingent.domain.area.repository.MoAddressRepository;
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
import org.springframework.util.StringUtils;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;
import jakarta.persistence.criteria.Path;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import jakarta.persistence.criteria.Subquery;

@Repository
@Transactional(propagation = Propagation.MANDATORY)
public class MoAddressRepositoryImpl extends BaseRepository implements MoAddressRepository {

    @Autowired
    private AddressesCRUDRepository addressesCRUDRepository;

    @Autowired
    private MoAddressPagingAndSortingRepository moAddressPagingAndSortingRepository;

    @Override
    public Page<MoAddress> getActiveMoAddresses(long moId, List<Long> areaTypeCodes, Pageable paging) {
        Specification<MoAddress> spec = (Specification<MoAddress>) (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                        criteriaBuilder.equal(root.get(MoAddress_.moId.getName()), moId),
                        areaTypeCodes == null || areaTypeCodes.isEmpty() ? criteriaBuilder.conjunction() :
                                criteriaBuilder.in(root.get(MoAddress_.areaType.getName()).get(AreaType_.code.getName())).value(areaTypeCodes), //root.get(MoAddress_.areaType.getName()).get(AreaType_.code.getName()).in(areaTypeCodes),
                        criteriaBuilder.or(
                                criteriaBuilder.greaterThanOrEqualTo(root.get(MoAddress_.endDate.getName()), LocalDate.now()),
                                root.get(MoAddress_.endDate.getName()).isNull()
                        )
                );
        if (paging == null) {
            return new PageImpl<>(moAddressPagingAndSortingRepository.findAll(spec, Sort.by(MoAddress_.id.getName()).ascending()));
        }
        return moAddressPagingAndSortingRepository.findAll(spec,
                PageRequest.of(paging.getPageNumber(), paging.getPageSize(), Sort.by(MoAddress_.id.getName()).ascending()));
    }

    @Override
    public List<MoAddress> getActiveMoAddresses(AreaType areaType) {
        Specification<MoAddress> moAddressSpecification = (root, criteriaQuery, criteriaBuilder) -> {
            //Принудительная загрузка адресов (отмена Fetch.LAZY), для ускорения выборки
            root.fetch(MoAddress_.address, JoinType.INNER);

            return criteriaBuilder.and(
                    criteriaBuilder.equal(root.get(MoAddress_.areaType.getName()), areaType.getCode()),
                    criteriaBuilder.or(
                            criteriaBuilder.greaterThanOrEqualTo(root.get(MoAddress_.endDate.getName()), LocalDate.now()),
                            root.get(MoAddress_.endDate.getName()).isNull()
                    ));
        };
        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification);
    }

    @Override
    public List<MoAddress> getActiveMoAddressByGlobalId(Long globalId, AreaType areaType) {
        Specification<MoAddress> moAddressSpecification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<MoAddress, Addresses> addressesJoin = root.join(MoAddress_.address, JoinType.INNER);
            return criteriaBuilder.and(
                    criteriaBuilder.equal(addressesJoin.get(Addresses_.globalId.getName()), globalId),
                    criteriaBuilder.equal(root.get(MoAddress_.areaType.getName()), areaType.getCode()),
                    criteriaBuilder.or(
                            criteriaBuilder.greaterThanOrEqualTo(root.get(MoAddress_.endDate.getName()), LocalDate.now()),
                            root.get(MoAddress_.endDate.getName()).isNull()
                    ));
        };
        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification);
    }

    @Override
    public List<MoAddress> getActiveMoAddressByGlobalIdAndAreaTypeCode(Long globalId, Long areaType) {
        Specification<MoAddress> moAddressSpecification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<MoAddress, Addresses> addressesJoin = root.join(MoAddress_.address, JoinType.INNER);
            return criteriaBuilder.and(
                    criteriaBuilder.equal(addressesJoin.get(Addresses_.globalId.getName()), globalId),
                    criteriaBuilder.equal(root.get(MoAddress_.areaType.getName()), areaType),
                    root.get(MoAddress_.endDate.getName()).isNull());
        };
        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification);
    }

    @Override
    public List<MoAddress> getActiveMoAddressByGlobalIdV3(AreaType areaType, Long moId, Addresses addresses) {
        Specification<MoAddress> moAddressSpecification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<MoAddress, Addresses> addressesJoin = root.join(MoAddress_.address, JoinType.INNER);

            return criteriaBuilder.and(
                    criteriaBuilder.equal(root.get(MoAddress_.areaType.getName()), areaType.getCode()),
                    criteriaBuilder.equal(root.get(MoAddress_.moId.getName()), moId),
                    root.get(MoAddress_.endDate.getName()).isNull(),
                    criteriaBuilder.equal(addressesJoin.get(Addresses_.globalId.getName()), addresses.getGlobalId())
            );
        };

        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification);
    }

    private Predicate getPredicateMoAddressAoLevelFullFind8(Addresses addresses, Join<MoAddress, Addresses> addressesJoin, CriteriaBuilder criteriaBuilder) {
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

    private Predicate getPredicateMoAddressAoLevelFullFind7(Addresses addresses, Join<MoAddress, Addresses> addressesJoin, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(addressesJoin.get(Addresses_.streetCode.getName()).isNotNull());
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.streetCode.getName()), addresses.getStreetCode()));
        predicates.add(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()).isNotNull());
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()), addresses.getAreaCodeOmkTe()));
//        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "65"));

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

    private Predicate getPredicateMoAddressAoLevelFullFind65(Addresses addresses, Join<MoAddress, Addresses> addressesJoin, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()).isNotNull());
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()), addresses.getAreaCodeOmkTe()));
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

    private Predicate getPredicateMoAddressAoLevelFullFind6(Addresses addresses, Join<MoAddress, Addresses> addressesJoin, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()).isNotNull());
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()), addresses.getAreaCodeOmkTe()));
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

    private Predicate getPredicateMoAddressAoLevelFullFind4(Addresses addresses, Join<MoAddress, Addresses> addressesJoin, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()).isNotNull());
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()), addresses.getAreaCodeOmkTe()));
        if (addresses.getCityCode() != null) {
            predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.cityCode.getName()), addresses.getCityCode()));
        } else {
            predicates.add(addressesJoin.get(Addresses_.cityCode.getName()).isNull());
        }

        return criteriaBuilder.and(predicates.toArray(new Predicate[]{}));
    }

    private Predicate getPredicateMoAddressAoLevelFullFind25(Addresses addresses, Join<MoAddress, Addresses> addressesJoin, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()).isNotNull());
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.areaCodeOmkTe.getName()), addresses.getAreaCodeOmkTe()));

        return criteriaBuilder.and(predicates.toArray(new Predicate[]{}));
    }

    private Predicate getPredicateMoAddressAoLevelFullFind2(Addresses addresses, Join<MoAddress, Addresses> addressesJoin, CriteriaBuilder criteriaBuilder) {
        List<Predicate> predicates = new ArrayList<>();
        predicates.add(addressesJoin.get(Addresses_.regionTeCode.getName()).isNotNull());
        predicates.add(criteriaBuilder.equal(addressesJoin.get(Addresses_.regionTeCode.getName()), addresses.getRegionTeCode()));

        return criteriaBuilder.and(predicates.toArray(new Predicate[]{}));
    }

    private Predicate getPredicateMoAddressAoLevel7(Addresses addresses, Join<MoAddress, Addresses> addressesJoin, CriteriaBuilder criteriaBuilder) {
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

    private Predicate getPredicateMoAddressCheckedLevel65(Addresses addresses, Join<MoAddress, Addresses> addressesJoin, CriteriaBuilder criteriaBuilder) {
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

    private Predicate getPredicateMoAddressCheckedLevel6(Addresses addresses, Join<MoAddress, Addresses> addressesJoin, CriteriaBuilder criteriaBuilder) {
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

    private Predicate getPredicateMoAddressCheckedLevel4(Addresses addresses, Join<MoAddress, Addresses> addressesJoin, CriteriaBuilder criteriaBuilder) {
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

    @Override
    public List<MoAddress> getActiveMoAddressLevel8(AreaType areaType, Long moId, Addresses addresses) {
        Specification<MoAddress> moAddressSpecification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<MoAddress, Addresses> addressesJoin = root.join(MoAddress_.address, JoinType.INNER);

            return criteriaBuilder.and(
                    criteriaBuilder.equal(root.get(MoAddress_.areaType.getName()), areaType.getCode()),
                    moId == null ? criteriaBuilder.conjunction() : criteriaBuilder.equal(root.get(MoAddress_.moId.getName()), moId),
                    root.get(MoAddress_.endDate.getName()).isNull(),

                    criteriaBuilder.or(
                            moId == null ? getPredicateMoAddressAoLevelFullFind8(addresses, addressesJoin, criteriaBuilder) :
                                    getPredicateMoAddressAoLevel7(addresses, addressesJoin, criteriaBuilder),
                            getPredicateMoAddressCheckedLevel65(addresses, addressesJoin, criteriaBuilder),
                            getPredicateMoAddressCheckedLevel6(addresses, addressesJoin, criteriaBuilder),
                            getPredicateMoAddressCheckedLevel4(addresses, addressesJoin, criteriaBuilder),
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
                    )
            );
        };

        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification);
    }

    @Override
    public List<MoAddress> getActiveMoAddressLevel7(AreaType areaType, Long moId, Addresses addresses) {
        Specification<MoAddress> moAddressSpecification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<MoAddress, Addresses> addressesJoin = root.join(MoAddress_.address, JoinType.INNER);

            return criteriaBuilder.and(
                    criteriaBuilder.equal(root.get(MoAddress_.areaType.getName()), areaType.getCode()),
                    moId == null ? criteriaBuilder.conjunction() : criteriaBuilder.equal(root.get(MoAddress_.moId.getName()), moId),
                    root.get(MoAddress_.endDate.getName()).isNull(),

                    criteriaBuilder.or(
                            moId == null ? getPredicateMoAddressAoLevelFullFind7(addresses, addressesJoin, criteriaBuilder) :
                                    getPredicateMoAddressCheckedLevel65(addresses, addressesJoin, criteriaBuilder),
                            moId == null ? getPredicateMoAddressCheckedLevel65(addresses, addressesJoin, criteriaBuilder) : criteriaBuilder.disjunction(),

                            getPredicateMoAddressCheckedLevel6(addresses, addressesJoin, criteriaBuilder),
                            getPredicateMoAddressCheckedLevel4(addresses, addressesJoin, criteriaBuilder),
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
                    )
            );
        };

        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification);
    }

    @Override
    public List<MoAddress> getActiveMoAddressLevel65(AreaType areaType, Long moId, Addresses addresses) {
        Specification<MoAddress> moAddressSpecification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<MoAddress, Addresses> addressesJoin = root.join(MoAddress_.address, JoinType.INNER);

            return criteriaBuilder.and(
                    criteriaBuilder.equal(root.get(MoAddress_.areaType.getName()), areaType.getCode()),
                    moId == null ? criteriaBuilder.conjunction() : criteriaBuilder.equal(root.get(MoAddress_.moId.getName()), moId),
                    root.get(MoAddress_.endDate.getName()).isNull(),

                    criteriaBuilder.or(
                            moId == null ? getPredicateMoAddressAoLevelFullFind65(addresses, addressesJoin, criteriaBuilder) :
                                    getPredicateMoAddressCheckedLevel6(addresses, addressesJoin, criteriaBuilder),

                            getPredicateMoAddressCheckedLevel4(addresses, addressesJoin, criteriaBuilder),
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
                    )
            );
        };

        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification);
    }

    @Override
    public List<MoAddress> getActiveMoAddressLevel6(AreaType areaType, Long moId, Addresses addresses) {
        Specification<MoAddress> moAddressSpecification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<MoAddress, Addresses> addressesJoin = root.join(MoAddress_.address, JoinType.INNER);

            return criteriaBuilder.and(
                    criteriaBuilder.equal(root.get(MoAddress_.areaType.getName()), areaType.getCode()),
                    moId == null ? criteriaBuilder.conjunction() : criteriaBuilder.equal(root.get(MoAddress_.moId.getName()), moId),
                    root.get(MoAddress_.endDate.getName()).isNull(),

                    criteriaBuilder.or(

                            moId == null ? getPredicateMoAddressAoLevelFullFind6(addresses, addressesJoin, criteriaBuilder) :
                                    getPredicateMoAddressCheckedLevel4(addresses, addressesJoin, criteriaBuilder),

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

                    )
            );
        };

        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification);
    }

    @Override
    public List<MoAddress> getActiveMoAddressLevel4(AreaType areaType, Long moId, Addresses addresses) {
        Specification<MoAddress> moAddressSpecification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<MoAddress, Addresses> addressesJoin = root.join(MoAddress_.address, JoinType.INNER);

            return criteriaBuilder.and(
                    criteriaBuilder.equal(root.get(MoAddress_.areaType.getName()), areaType.getCode()),
                    moId == null ? criteriaBuilder.conjunction() : criteriaBuilder.equal(root.get(MoAddress_.moId.getName()), moId),
                    root.get(MoAddress_.endDate.getName()).isNull(),

                    criteriaBuilder.or(
                            moId == null ? getPredicateMoAddressAoLevelFullFind4(addresses, addressesJoin, criteriaBuilder) :

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
                    )
            );
        };

        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification);
    }

    @Override
    public List<MoAddress> getActiveMoAddressLevel25(AreaType areaType, Long moId, Addresses addresses) {
        Specification<MoAddress> moAddressSpecification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<MoAddress, Addresses> addressesJoin = root.join(MoAddress_.address, JoinType.INNER);

            return criteriaBuilder.and(
                    criteriaBuilder.equal(root.get(MoAddress_.areaType.getName()), areaType.getCode()),
                    moId == null ? criteriaBuilder.conjunction() : criteriaBuilder.equal(root.get(MoAddress_.moId.getName()), moId),
                    root.get(MoAddress_.endDate.getName()).isNull(),

                    criteriaBuilder.or(
                            moId == null ? getPredicateMoAddressAoLevelFullFind25(addresses, addressesJoin, criteriaBuilder) :

                            criteriaBuilder.and(
                                    addressesJoin.get(Addresses_.regionTeCode.getName()).isNotNull(),
                                    criteriaBuilder.equal(addressesJoin.get(Addresses_.regionTeCode.getName()), addresses.getRegionTeCode()),
                                    criteriaBuilder.equal(addressesJoin.get(Addresses_.aoLevel.getName()), "2")
                            )
                    )
            );
        };

        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification);
    }

    @Override
    public List<MoAddress> getActiveMoAddressLevel2(AreaType areaType, Long moId, Addresses addresses) {
        Specification<MoAddress> moAddressSpecification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<MoAddress, Addresses> addressesJoin = root.join(MoAddress_.address, JoinType.INNER);

            return criteriaBuilder.and(
                    criteriaBuilder.equal(root.get(MoAddress_.areaType.getName()), areaType.getCode()),
                    root.get(MoAddress_.endDate.getName()).isNull(),

                    moId == null ? criteriaBuilder.conjunction() : criteriaBuilder.equal(root.get(MoAddress_.moId.getName()), moId),
                    moId == null ? getPredicateMoAddressAoLevelFullFind2(addresses, addressesJoin, criteriaBuilder) : criteriaBuilder.disjunction()
            );
        };

        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification);
    }

    @Override
    public Page<MoAddress> getActiveMoAddressesByGlobalIds(List<Long> globalIds) {
        if (globalIds.isEmpty()) {
            return Page.empty();
        }
        Specification<MoAddress> moAddressSpecification = (root, query, builder) -> {
            Join<MoAddress, Addresses> addressesJoin = root.join(MoAddress_.address, JoinType.INNER);
            LocalDate now = LocalDate.now();

            return builder.and(
                    builder.in(addressesJoin.get(Addresses_.globalId.getName())).value(globalIds),
                    builder.or(
                            builder.lessThanOrEqualTo(root.get(MoAddress_.startDate.getName()), now),
                            root.get(MoAddress_.startDate.getName()).isNull()
                    ),
                    builder.or(
                            builder.greaterThanOrEqualTo(root.get(MoAddress_.endDate.getName()), now),
                            root.get(MoAddress_.endDate.getName()).isNull()
                    )
            );
        };
        Sort sorting = Sort.by(MoAddress_.address.getName() + "." + Addresses_.globalId.getName());

        return new PageImpl<>(moAddressPagingAndSortingRepository.findAll(moAddressSpecification, sorting));
    }

    @Override
    public Page<MoAddress> getActiveMoAddressesByGlobalIds(List<Long> globalIds, Pageable paging) {
        if (globalIds.isEmpty()) {
            return Page.empty();
        }
        LocalDate now = LocalDate.now();

        Specification<Addresses> addressesSpecification = (root, query, cb) -> {
            Subquery<MoAddress> sub = query.subquery(MoAddress.class);
            Root<MoAddress> subRoot = sub.from(MoAddress.class);

            sub.where(cb.and(
                    cb.equal(subRoot.get(MoAddress_.address.getName()), root.get(Addresses_.id.getName())),
                    cb.or(
                            cb.lessThanOrEqualTo(subRoot.get(MoAddress_.startDate.getName()), now),
                            subRoot.get(MoAddress_.startDate.getName()).isNull()
                    ),
                    cb.or(
                            cb.greaterThanOrEqualTo(subRoot.get(MoAddress_.endDate.getName()), now),
                            subRoot.get(MoAddress_.endDate.getName()).isNull()
                    )
            )).select(subRoot);

            return cb.and(
                    cb.in(root.get(Addresses_.globalId.getName())).value(globalIds),
                    cb.exists(sub)
            );
        };
        Sort sorting = Sort.by(Addresses_.globalId.getName());
        Page<Addresses> addresses = addressesCRUDRepository.findAll(addressesSpecification,
                PageRequest.of(paging.getPageNumber(), paging.getPageSize(), sorting));

        if (addresses.isEmpty()) {
            return Page.empty();
        }
        Specification<MoAddress> moAddressSpecification = (root, query, builder) -> {
            Join<MoAddress, Addresses> addressesJoin = root.join(MoAddress_.address, JoinType.INNER);

            return builder.and(
                    builder.in(addressesJoin.get(Addresses_.id.getName())).value(addresses.getContent().stream()
                            .map(Addresses::getId)
                            .collect(Collectors.toList())),
                    builder.or(
                            builder.lessThanOrEqualTo(root.get(MoAddress_.startDate.getName()), now),
                            root.get(MoAddress_.startDate.getName()).isNull()
                    ),
                    builder.or(
                            builder.greaterThanOrEqualTo(root.get(MoAddress_.endDate.getName()), now),
                            root.get(MoAddress_.endDate.getName()).isNull()
                    )
            );
        };
        sorting = Sort.by(MoAddress_.address.getName() + "." + Addresses_.globalId.getName());

        return new PageImplCustom<>(moAddressPagingAndSortingRepository.findAll(moAddressSpecification, sorting),
                paging, addresses.getTotalElements());
    }

    @Override
    public List<MoAddress> getActiveMoAddressByGlobalIdAndLevel(Long globalId, String aoLevel, AreaType areaType) {
        Specification<MoAddress> moAddressSpecification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<MoAddress, Addresses> addressesJoin = root.join(MoAddress_.address, JoinType.INNER);
            return criteriaBuilder.and(
                    criteriaBuilder.or(
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.globalId.getName()), globalId),
                            criteriaBuilder.notEqual(addressesJoin.get(Addresses_.aoLevel.getName()), aoLevel)
                    ),
                    criteriaBuilder.equal(root.get(MoAddress_.areaType.getName()), areaType.getCode()),
                    criteriaBuilder.or(
                            criteriaBuilder.greaterThanOrEqualTo(root.get(MoAddress_.endDate.getName()), LocalDate.now()),
                            criteriaBuilder.isNull(root.get(MoAddress_.endDate.getName()))
                    ));
        };
        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification);
    }

    @Override
    public Optional<MoAddress> findById(Long id) {
        return moAddressPagingAndSortingRepository.findById(id);
    }

    @Override
    public void delete(MoAddress moAddress) {
        moAddressPagingAndSortingRepository.delete(moAddress);
    }

    @Override
    public MoAddress save(MoAddress moAddress) {
        return moAddressPagingAndSortingRepository.save(moAddress);
    }

    @Override
    public List<MoAddress> saveAll(List<MoAddress> moAddress) {
        return moAddressPagingAndSortingRepository.saveAll(moAddress);
    }

    @Override
    public Page<MoAddress> find(long moId, List<Long> addressGlobalIds, List<Long> areaTypeCodes, LocalDate orderDate,
                                String orderName, String orderNumber, String orderOuz, LocalDate orderCreateDate, PageRequest paging) {
        Specification<MoAddress> specification = (root, query, builder) -> {

            Join<MoAddress, Addresses> moAddressAddressJoin = root.join(MoAddress_.address, JoinType.INNER);
            Join<MoAddress, AddressAllocationOrders> moAddressAddressAllocationOrdersJoin =
                    root.join(MoAddress_.addressAllocationOrder, JoinType.LEFT);
            Join<MoAddress, AddressAllocationOrders> moAddressAddressRejectAllocationOrdersJoin =
                    root.join(MoAddress_.addressRejectOrder, JoinType.LEFT);

            LocalDate now = LocalDate.now();

            return builder.and(
                    builder.equal(root.get(MoAddress_.moId), moId),
                    areaTypeCodes == null || areaTypeCodes.isEmpty() ? builder.conjunction() :
                            builder.in(root.get(MoAddress_.areaType).get(AreaType_.code.getName())).value(areaTypeCodes),
                    builder.or(
                            builder.lessThanOrEqualTo(root.get(MoAddress_.startDate.getName()), now),
                            root.get(MoAddress_.startDate.getName()).isNull()
                    ),
                    builder.or(
                            builder.greaterThan(root.get(MoAddress_.endDate.getName()), now),
                            root.get(MoAddress_.endDate.getName()).isNull()
                    ),
                    addressGlobalIds == null || addressGlobalIds.isEmpty() ? builder.conjunction() :
                            builder.in(root.get(MoAddress_.address).get(Addresses_.globalId.getName())).value(addressGlobalIds),
                    orderDate == null ? builder.conjunction() :
                            builder.equal(root.get(MoAddress_.addressAllocationOrder).get(AddressAllocationOrders_.date), orderDate),
                    builder.or(
                            buildAllocationOrderPredicate(builder, root.get(MoAddress_.addressAllocationOrder),
                                    orderName, orderNumber, orderOuz, orderCreateDate),
                            buildAllocationOrderPredicate(builder, root.get(MoAddress_.addressRejectOrder),
                                    orderName, orderNumber, orderOuz, orderCreateDate)
                    )
            );
        };
        return paging == null ? new PageImpl<>(moAddressPagingAndSortingRepository.findAll(specification)) :
                moAddressPagingAndSortingRepository.findAll(specification, paging);
    }

    private Predicate buildAllocationOrderPredicate(CriteriaBuilder builder, Path<AddressAllocationOrders> root,
                                                    String orderName, String orderNumber, String orderOuz, LocalDate orderCreateDate) {
        List<Predicate> predicates = new ArrayList<>();

        if (StringUtils.hasText(orderNumber)) {
            predicates.add(builder.equal(root.get(AddressAllocationOrders_.number), orderNumber));
        }
        if (orderCreateDate != null) {
            predicates.add(builder.equal(builder.function("DATE", LocalDate.class, root.get(AddressAllocationOrders_.createDate)), orderCreateDate));
        }
        if (StringUtils.hasText(orderName)) {
            predicates.add(builder.like(builder.lower(root.get(AddressAllocationOrders_.name)), "%" + orderName.toLowerCase() + "%"));
        }
        if (StringUtils.hasText(orderOuz)) {
            predicates.add(builder.like(builder.lower(root.get(AddressAllocationOrders_.ouz)), "%" + orderOuz.toLowerCase() + "%"));
        }
        return predicates.isEmpty() ? builder.conjunction() : builder.and(predicates.toArray(new Predicate[0]));
    }
}
