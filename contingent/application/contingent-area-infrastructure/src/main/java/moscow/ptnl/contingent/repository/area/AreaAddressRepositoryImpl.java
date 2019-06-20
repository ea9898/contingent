package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.area.AreaAddress_;
import moscow.ptnl.contingent.area.entity.area.Area_;
import moscow.ptnl.contingent.area.entity.area.MoAddress_;
import moscow.ptnl.contingent.area.entity.nsi.AreaType_;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class AreaAddressRepositoryImpl extends BaseRepository implements AreaAddressRepository {

    @Autowired
    AreaAddressCRUDRepository areaAddressCRUDRepository;

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

    @Override
    public List<AreaAddress> getActiveAreaAddresses(long moId, long areaTypeCode) {
        Specification<AreaAddress> specification = (Specification<AreaAddress>) (root, criteriaQuery, criteriaBuilder) ->
            criteriaBuilder.and(
                criteriaBuilder.equal(root.get(AreaAddress_.area.getName()).get(Area_.moId.getName()), moId),
                criteriaBuilder.equal(root.get(AreaAddress_.area.getName()).get(Area_.areaType.getName()).get(AreaType_.code.getName()), areaTypeCode)
            );

        specification = specification.and(activeAreaAddressesSpec());
        return areaAddressCRUDRepository.findAll(specification);
    }

    @Override
    public List<AreaAddress> findAreaAddresses(List<Long> moAddressIds) {
        return areaAddressCRUDRepository.findAll(findMoAddressesByIdsSpec(moAddressIds));
    }

    @Override
    public List<AreaAddress> findAreaAddressesActual(List<Long> moAddressIds) {
        return areaAddressCRUDRepository.findAll(findMoAddressesByIdsSpec(moAddressIds).and(activeAreaAddressesSpec()));
    }

    @Override
    public List<AreaAddress> findAreaAddressesByAreaId(long areaId) {
        Specification<AreaAddress> specification = (root, criteriaQuery, criteriaBuilder) ->
            criteriaBuilder.equal(root.get(AreaAddress_.area.getName()), areaId);

        return areaAddressCRUDRepository.findAll(specification.and(activeAreaAddressesSpec()));
    }

    @Override
    public List<AreaAddress> findAreaAddressByAddress(Addresses addresses) {
        Specification<AreaAddress> specification = (root, criteriaQuery, criteriaBuilder) ->
            criteriaBuilder.equal(root.get(AreaAddress_.address.getName()), addresses);

        return areaAddressCRUDRepository.findAll(specification.and(activeAreaAddressesSpec()));
    }
}
