package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress_;
import moscow.ptnl.contingent.domain.area.entity.Area_;
import moscow.ptnl.contingent.domain.area.entity.MoAddress_;
import moscow.ptnl.contingent.domain.area.repository.AreaAddressRepository;
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
import java.util.List;
import moscow.ptnl.contingent.domain.area.entity.Addresses_;
import moscow.ptnl.contingent.nsi.domain.area.AreaType_;
import moscow.ptnl.contingent.repository.CommonSpecification;

import javax.persistence.criteria.JoinType;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
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
                        cb.or(
                                cb.greaterThanOrEqualTo(root.get(AreaAddress_.endDate), LocalDate.now()),
                                cb.isNull(root.get(AreaAddress_.endDate))
                        )
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
}
