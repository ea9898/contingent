package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.area.AreaAddress_;
import moscow.ptnl.contingent.area.entity.area.Area_;
import moscow.ptnl.contingent.area.entity.area.MoAddress_;
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
import moscow.ptnl.contingent.nsi.domain.area.AreaType_;

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

    // Спека поиска территорий обслуживания по ID
    private Specification<AreaAddress> findMoAddressesByIdsSpec(List<Long> moAddressIds) {
        return (Specification<AreaAddress>) (root, criteriaQuery, criteriaBuilder) ->
                root.get(AreaAddress_.moAddress.getName()).get(MoAddress_.id.getName()).in(moAddressIds);
    }

    private Specification<AreaAddress> findAreaAddressesByIdsSpec(List<Long> areaAddressIds) {
        return (Specification<AreaAddress>) (root, criteriaQuery, criteriaBuilder) ->
                root.get(AreaAddress_.id.getName()).in(areaAddressIds);
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
    public List<AreaAddress> findAreaAddressesActual(List<Long> areaAddressIds) {
        return areaAddressPagingAndSortingRepository.findAll(findAreaAddressesByIdsSpec(areaAddressIds).and(activeAreaAddressesSpec()));
    }

    @Override
    public Page<AreaAddress> findAreaAddressesByAreaId(long areaId, Pageable paging) {
        Specification<AreaAddress> specification = (root, criteriaQuery, criteriaBuilder) ->
            criteriaBuilder.equal(root.get(AreaAddress_.area.getName()).get(Area_.id.getName()), areaId);
        specification = specification.and(activeAreaAddressesSpec());


        if (paging == null) {
            return new PageImpl<>(areaAddressPagingAndSortingRepository.findAll(specification, Sort.by(AreaAddress_.id.getName()).ascending()));
        }
        return areaAddressPagingAndSortingRepository.findAll(specification,
                PageRequest.of(paging.getPageNumber(), paging.getPageSize(), Sort.by(AreaAddress_.id.getName()).ascending()));
    }

    @Override
    public List<AreaAddress> findAreaAddressByAddressIds(List<Long> addressIds) {
        Specification<AreaAddress> specification = (root, criteriaQuery, cb) ->
                cb.and(
                        root.get(AreaAddress_.address).in(addressIds),
                        cb.or(
                                cb.greaterThanOrEqualTo(root.get(AreaAddress_.endDate), LocalDate.now()),
                                cb.isNull(root.get(AreaAddress_.endDate))
                        )
                );

        return areaAddressPagingAndSortingRepository.findAll(specification);
    }
}
