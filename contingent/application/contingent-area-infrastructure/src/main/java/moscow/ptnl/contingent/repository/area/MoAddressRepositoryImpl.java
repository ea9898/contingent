package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders_;
import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.Addresses_;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.domain.area.entity.MoAddress_;
import moscow.ptnl.contingent.domain.area.repository.MoAddressRepository;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.repository.BaseRepository;
import moscow.ptnl.contingent2.area.info.Address;
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
import java.util.Optional;

import moscow.ptnl.contingent.nsi.domain.area.AreaType_;
import org.springframework.util.StringUtils;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class MoAddressRepositoryImpl extends BaseRepository implements MoAddressRepository {

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
    public List<MoAddress> getActiveMoAddressesByGlobalIds(List<Long> globalIds) {
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
        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification, Sort.by(MoAddress_.address.getName() + "." + Addresses_.globalId.getName()));
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
                            root.get(MoAddress_.endDate.getName()).isNull()
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
