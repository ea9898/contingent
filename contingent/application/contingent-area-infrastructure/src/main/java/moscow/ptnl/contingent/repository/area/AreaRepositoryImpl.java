package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.Addresses_;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress_;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees_;
import moscow.ptnl.contingent.domain.area.entity.AreaToAreaType;
import moscow.ptnl.contingent.domain.area.entity.AreaToAreaType_;
import moscow.ptnl.contingent.domain.area.entity.Area_;
import moscow.ptnl.contingent.domain.area.repository.AreaRepository;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeSpecializations;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeSpecializations_;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.jdbc.support.incrementer.PostgresSequenceMaxValueIncrementer;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.PostConstruct;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;
import javax.sql.DataSource;
import java.time.LocalDate;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import moscow.ptnl.contingent.nsi.domain.area.AreaType_;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class AreaRepositoryImpl extends BaseRepository implements AreaRepository {
    
    private final static Logger LOG = LoggerFactory.getLogger(AreaRepositoryImpl.class);

    @Autowired
    private DataSource dataSource;
    
    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    private PostgresSequenceMaxValueIncrementer areaSequenceIncrementer;

    private Specification<Area> searchByMuIdSpec(Long muId) {
        return (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(Area_.muId.getName()), muId);
    }

    private Specification<Area> searchByMoIdSpec(Long moId) {
        return (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(Area_.moId.getName()), moId);
    }

    private Specification<Area> searchByMuIdsSpec(List<Long> muIds) {
        return (root, criteriaQuery, cb) -> root.get(Area_.muId.getName()).in(muIds);
    }

    private Specification<Area> searchByAreaTypeCodesSpec(List<Long> areaTypeCodes) {
        return (root, criteriaQuery, cb) -> root.get(Area_.areaType.getName()).in(areaTypeCodes);
    }

    private Specification<Area> searchByAreaIdsSpec(List<Long> areaIds) {
        return (root, criteriaQuery, cb) -> root.get(Area_.id.getName()).in(areaIds);
    }

    private Specification<Area> searchWithActualMainEmployeesSpec() {
        return (root, criteriaQuery, cb) -> {
            Subquery<AreaMedicalEmployees> sub = criteriaQuery.subquery(AreaMedicalEmployees.class);
            Root<AreaMedicalEmployees> subRoot = sub.from(AreaMedicalEmployees.class);
            return cb.exists(sub.where(cb.and(
                    cb.equal(subRoot.get(AreaMedicalEmployees_.area.getName()), root.get(Area_.id.getName())),
                    cb.equal(subRoot.get(AreaMedicalEmployees_.replacement.getName()), false),
                    cb.or(
                        cb.isNull(subRoot.get(AreaMedicalEmployees_.endDate)),
                        cb.greaterThanOrEqualTo(subRoot.get(AreaMedicalEmployees_.endDate.getName()), LocalDate.now())
                    ),
                    cb.or(
                        cb.isNull(subRoot.get(AreaMedicalEmployees_.isError)),
                        cb.equal(subRoot.get(AreaMedicalEmployees_.isError), false)
                    )
            )).select(subRoot));
        };
    }

    private Specification<Area> searchBySpecializationCodesSpec(List<Long> specializationCodes) {
        return (root, criteriaQuery, cb) -> {
            Subquery<AreaTypeSpecializations> sub = criteriaQuery.subquery(AreaTypeSpecializations.class);
            Root<AreaTypeSpecializations> subRoot = sub.from(AreaTypeSpecializations.class);
            return cb.exists(sub.where(cb.and(
                    cb.equal(subRoot.get(AreaTypeSpecializations_.areaType.getName()), root.get(Area_.areaType.getName())),
                    subRoot.get(AreaTypeSpecializations_.specializationCode.getName()).in(specializationCodes)
            )).select(subRoot));
        };
    }

    private Specification<Area> searchEmptyMuIdSpec() {
        return (root, criteriaQuery, criteriaBuilder) ->
                root.get(Area_.muId.getName()).isNull();
    }

    private Specification<Area> searchActive() {
        return (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(Area_.archived.getName()), false);
    }

    @PostConstruct
    private void initialize() {
        areaSequenceIncrementer = new PostgresSequenceMaxValueIncrementer(dataSource, "SEQ_AREAS");
    }

    @Override
    public List<Area> findAreasForSyncToK1(int daysForSelect) {
        Specification<Area> specification = (root, criteriaQuery, cb) -> {
            Subquery<AreaMedicalEmployees> sub = criteriaQuery.subquery(AreaMedicalEmployees.class);
            Root<AreaMedicalEmployees> subRoot = sub.from(AreaMedicalEmployees.class);
            sub.where(cb.and(
                    cb.equal(subRoot.get(AreaMedicalEmployees_.area), root.get(Area_.id)),
                    cb.lessThan(subRoot.get(AreaMedicalEmployees_.endDate), LocalDate.now()),
                    cb.greaterThanOrEqualTo(subRoot.get(AreaMedicalEmployees_.endDate), LocalDate.now().minusDays(daysForSelect)),
                    cb.greaterThanOrEqualTo(subRoot.get(AreaMedicalEmployees_.endDate), root.get(Area_.updateDate.getName())),
                    cb.or(
                            cb.isNull(subRoot.get(AreaMedicalEmployees_.isError)),
                            cb.equal(subRoot.get(AreaMedicalEmployees_.isError), false)
                    )
            ));
            return cb.exists(sub.select(subRoot));
        };
        return areaCRUDRepository.findAll(specification);
    }

    @Override
    public List<Area> findAreas(Long moId, Long muId, List<Long> areaTypeCodes, Integer number, Boolean actual) {
        Specification<Area> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                        moId == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(Area_.moId.getName()), moId),
                        muId == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(Area_.muId.getName()), muId),
                        number == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(Area_.number.getName()), number),
                        areaTypeCodes == null || areaTypeCodes.isEmpty() ? criteriaBuilder.conjunction() :
                                root.get(Area_.areaType.getName()).get(AreaType_.code.getName()).in(areaTypeCodes),
                        actual == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(Area_.archived.getName()), !actual));
        return areaCRUDRepository.findAll(specification);
    }

    @Override
    public List<Area> findAreas(Long areaTypeClassCode, Long moId, List<Long> muIds, List<Long> areaTypeCodes,
                                Integer number, String description, Boolean archived) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Area> criteria = criteriaBuilder.createQuery(Area.class);
        Root<Area> root = criteria.from(Area.class);
        Join<Area, AreaType> areaTypeJoin = root.join(Area_.areaType, JoinType.LEFT);
        criteria.select(root);
        criteria.where(
                criteriaBuilder.and(
                        areaTypeClassCode == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(areaTypeJoin.get(AreaType_.areaTypeClass), areaTypeClassCode),
                        moId == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(Area_.moId), moId),
                        muIds == null || muIds.isEmpty() ? criteriaBuilder.conjunction() :
                                root.get(Area_.muId).in(muIds),
                        areaTypeCodes == null || areaTypeCodes.isEmpty() ? criteriaBuilder.conjunction() :
                                root.get(Area_.areaType).get(AreaType_.code).in(areaTypeCodes),
                        number == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(Area_.number), number),
                        description == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.like(root.get(Area_.description), "%"+description+"%"),
                        archived == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(Area_.archived), archived))
        );
        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<Area> findAreas(Long areaTypeKindCode, Boolean archived, Long medicalEmployeeJobInfo) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Area> criteria = criteriaBuilder.createQuery(Area.class);
        //CriteriaQuery<Tuple> criteria = criteriaBuilder.createTupleQuery();
        Root<AreaMedicalEmployees> root = criteria.from(AreaMedicalEmployees.class);
        Join<AreaMedicalEmployees, Area> areaJoin  = root.join(AreaMedicalEmployees_.area, JoinType.LEFT);
        Join<Area, AreaType> areaTypeJoin = areaJoin.join(Area_.areaType, JoinType.LEFT);
        //criteria.select(criteriaBuilder.tuple(areaJoin.get(Area_.id), areaJoin.get(Area_.moId)));
        criteria.select(areaJoin);
        criteria.where(
                criteriaBuilder.and(
                        criteriaBuilder.equal(areaTypeJoin.get(AreaType_.areaTypeKind), areaTypeKindCode),
                        criteriaBuilder.equal(areaJoin.get(Area_.archived), archived),
                        criteriaBuilder.equal(root.get(AreaMedicalEmployees_.medicalEmployeeJobId), medicalEmployeeJobInfo))
        );
        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<Area> findAreas(Long moId, Long muId, Long areaTypeCode, Integer number, Boolean actual) {
        return findAreas(moId, muId, Collections.singletonList(areaTypeCode), number, actual);
    }

    @Override
    public List<Area> findDependentAreasByMuMoIdAndType(Long muId, Long moId, Long areaTypeCode, Long areaTypeKindCode) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Area> criteria = criteriaBuilder.createQuery(Area.class);
        Root<Area> root = criteria.from(Area.class);
        Join<Area, AreaToAreaType> areaAreaToAreaTypeJoin = root.join(Area_.primaryAreaTypes, JoinType.LEFT);
        criteria.select(root);
        criteria.where(
                criteriaBuilder.and(
                        muId == null
                                ? criteriaBuilder.and(
                                criteriaBuilder.isNull(root.get(Area_.muId.getName())),
                                criteriaBuilder.equal(root.get(Area_.moId.getName()), moId))
                                : criteriaBuilder.equal(root.get(Area_.muId.getName()), muId),
                        criteriaBuilder.equal(areaAreaToAreaTypeJoin.get(AreaToAreaType_.areaType.getName()), areaTypeCode),
                        criteriaBuilder.notEqual(root.get(Area_.areaType.getName()).get(AreaType_.areaTypeKind.getName()), areaTypeKindCode),
                        criteriaBuilder.equal(root.get(Area_.archived.getName()), false))
        );

        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<Area> findPrimaryAreasByAreaEqAreaType(Area area) {
        Specification<Area> specification = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.conjunction();
        if (area.getMuId() != null) {
            specification = specification.and(searchByMuIdSpec(area.getMuId()));
        } else {
            specification = specification.and(searchByMoIdSpec(area.getMoId()));
        }

        specification = specification.and(searchActive());

        List<Area> primArea = areaCRUDRepository.findAll(specification);

        return primArea
                .stream()
                .filter(da -> area.getPrimaryAreaTypes() != null &&
                        area.getPrimaryAreaTypes().stream().map(AreaToAreaType::getAreaType).anyMatch(a -> Objects.equals(a, da.getAreaType())))
                .collect(Collectors.toList());
    }

    @Override
    public Long getNextAreaId() {
        return areaSequenceIncrementer.nextLongValue();
    }

    @Override
    public Page<Area> findAreas(Long moId, List<Long> muIds, List<Long> areaTypeCodes, List<Long> specializationCodes, List<Long> areaIds, PageRequest paging) {
        Specification<Area> specification = searchWithActualMainEmployeesSpec();

        if (moId != null) {
            specification = specification.and(searchByMoIdSpec(moId));
        }
        if (!muIds.isEmpty()) {
            specification = specification.and(searchByMuIdsSpec(muIds));
        }
        if (!areaTypeCodes.isEmpty()) {
            specification = specification.and(searchByAreaTypeCodesSpec(areaTypeCodes));
        }
        if (!areaIds.isEmpty()) {
            specification = specification.and(searchByAreaIdsSpec(areaIds));
        }
        if (!specializationCodes.isEmpty()) {
            specification = specification.and(searchBySpecializationCodesSpec(specializationCodes));
        }
        specification = specification.and(searchActive());

        return areaCRUDRepository.findAll(specification, paging);
    }

    @Override
    public Page<Area> findActualAreasByAddressIds(List<Long> areaTypeCodes, List<Long> addressIds, PageRequest paging) {
        Specification<Area> specification = searchActive();

        if (areaTypeCodes != null && !areaTypeCodes.isEmpty()) {
            specification = specification.and(searchByAreaTypeCodesSpec(areaTypeCodes));
        }
        specification = specification.and((root, criteriaQuery, cb) -> {
            Join<Area, AreaAddress> areaAddressJoin = root.join(Area_.areaAddresses, JoinType.LEFT);
            return cb.and(
                    areaAddressJoin.get(AreaAddress_.address.getName()).get(Addresses_.id.getName()).in(addressIds),
                    cb.or(
                            cb.isNull(areaAddressJoin.get(AreaAddress_.endDate.getName())),
                            cb.greaterThan(areaAddressJoin.get(AreaAddress_.endDate.getName()), LocalDate.now())
                    )
            );
        });
        return areaCRUDRepository.findAll(specification, paging);
    }

    @Override
    public Page<Area> getAreas(List<Long> areaIds, PageRequest paging) {
        if (areaIds.isEmpty()) {
            return Page.empty();
        }
        return areaCRUDRepository.findAll(searchByAreaIdsSpec(areaIds), paging);
    }

    @Override
    public List<Area> findAllById(List<Long> areaIds) {
        return areaCRUDRepository.findAllById(areaIds);
    }

    @Override
    public Optional<Area> findById(Long areaId) { return areaCRUDRepository.findById(areaId); }

    @Override
    public Area save(Area area) {
        return areaCRUDRepository.save(area);
    }
}
