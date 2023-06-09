package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaMuService;
import moscow.ptnl.contingent.domain.area.entity.AreaMuService_;
import moscow.ptnl.contingent.domain.area.entity.Area_;
import moscow.ptnl.contingent.domain.area.repository.AreaMuServiceRepository;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeProfile_;
import moscow.ptnl.contingent.repository.BaseRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.Collection;
import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class AreaMuServiceRepositoryImpl extends BaseRepository implements AreaMuServiceRepository {

    @Autowired
    private AreaMuServiceCRUDRepository areaMuServiceCRUDRepository;

    private Specification<AreaMuService> buildMuIdSpec(long muId) {
        return (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(AreaMuService_.muId), muId);
    }

    private Specification<AreaMuService> buildMuIdsSpec(List<Long> muIds) {

        return (root, criteriaQuery, criteriaBuilder) -> root.get(AreaMuService_.muId).in(muIds);
    }

    private Specification<AreaMuService> buildAreaSpec(long areaId) {
        return (root, criteriaQuery, cb) -> cb.equal(root.get(AreaMuService_.area).get(Area_.id), areaId);
    }

    private Specification<AreaMuService> buildAreasSpec(Collection<Long> areaIds) {
        return (root, criteriaQuery, cb) -> cb.in(root.get(AreaMuService_.area).get(Area_.id.getName())).value(areaIds);
    }

    private Specification<AreaMuService> buildExcludeAreaSpec(long areaId) {
        return (root, criteriaQuery, cb) -> cb.notEqual(root.get(AreaMuService_.area).get(Area_.id), areaId);
    }

    private Specification<AreaMuService> buildAreaTypeProfileSpec(long areaTypeProfileCode) {
        return (root, criteriaQuery, cb) ->
                cb.equal(root.get(AreaMuService_.area).get(Area_.areaTypeProfile).get(AreaTypeProfile_.code), areaTypeProfileCode);
    }

    private Specification<AreaMuService> buildActiveEndDateSpec() {
        return (root, criteriaQuery, cb) -> cb.or(
                cb.isNull(root.get(AreaMuService_.endDate)),
                cb.greaterThanOrEqualTo(root.get(AreaMuService_.endDate), LocalDate.now())
        );
    }

    @Override
    public List<AreaMuService> findActive(Long muId, Long excludeAreaId, Long areaTypeProfileCode) {
        Specification<AreaMuService> specification = buildActiveEndDateSpec();
        specification = specification.and(buildMuIdSpec(muId));
        specification = specification.and(buildExcludeAreaSpec(excludeAreaId));
        specification = specification.and(buildAreaTypeProfileSpec(areaTypeProfileCode));
        return areaMuServiceCRUDRepository.findAll(specification);
    }

    @Override
    public List<AreaMuService> findActive(Long muId, Long areaId) {
        Specification<AreaMuService> specification = buildActiveEndDateSpec();
        specification = specification.and(buildMuIdSpec(muId));
        specification = specification.and(buildAreaSpec(areaId));
        return areaMuServiceCRUDRepository.findAll(specification);
    }

    @Override
    public void saveAll(List<AreaMuService> areaMuServices) {
        areaMuServiceCRUDRepository.saveAll(areaMuServices);
    }

    @Override
    public AreaMuService save(AreaMuService areaMuService) {
        return areaMuServiceCRUDRepository.save(areaMuService);
    }

    @Override
    public void closeAreaMuServices(Long servicedMuId, Area area, LocalDate searchEndDate, LocalDate newEndDate) {
        areaMuServiceCRUDRepository.closeAreaMuServices(servicedMuId, area, searchEndDate, newEndDate);
    }

    @Override
    public AreaMuService findById(Long id) {
        return areaMuServiceCRUDRepository.getOne(id);
    }

    @Override
    public List<AreaMuService> findActive(List<Long> areaIds) {
        Specification<AreaMuService> specification = buildActiveEndDateSpec();
        specification = specification.and(buildAreasSpec(areaIds));
        return areaMuServiceCRUDRepository.findAll(specification);
    }

    @Override
    public List<AreaMuService> findActiveByMuIds(List<Long> muIds) {
        Specification<AreaMuService> specification = buildActiveEndDateSpec();
        specification = specification.and(buildMuIdsSpec(muIds));
        return areaMuServiceCRUDRepository.findAll(specification);
    }
}
