package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.Area_;
import moscow.ptnl.contingent.area.entity.nsi.AreaType_;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.jdbc.support.incrementer.PostgresSequenceMaxValueIncrementer;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.PostConstruct;
import javax.sql.DataSource;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
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
                criteriaBuilder.equal(root.get(Area_.muId), muId);
    }

    private Specification<Area> searchByMoIdSpec(Long moId) {
        return (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(Area_.moId), moId);
    }

    private Specification<Area> searchEmptyMuIdSpec() {
        return (root, criteriaQuery, criteriaBuilder) ->
                root.get(Area_.muId).isNull();
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
    public List<Area> findAreasWithMuIdNullAndNotAreaTypeKindCode(Long moId, Long areaTypeCode, Long areaTypeKindCode, Integer number, Boolean actual) {
        Specification<Area> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                        moId == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(Area_.moId.getName()), moId),
                                criteriaBuilder.isNull(root.get(Area_.muId.getName())),
                        number == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(Area_.number.getName()), number),
                        areaTypeCode == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(Area_.areaType.getName()), areaTypeCode),
                        areaTypeKindCode == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.notEqual(root.get(Area_.areaType).get(AreaType_.areaTypeKind), areaTypeKindCode),
                        actual == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(Area_.archived.getName()), !actual));
        return areaCRUDRepository.findAll(specification);
    }

    @Override
    public List<Area> findAreas(Long moId, Long muId, Long areaTypeCode, Integer number, Boolean actual) {
        return findAreas(moId, muId, Collections.singletonList(areaTypeCode), number, actual);
    }

    @Override
    public List<Area> findAreasWithNotAreaTypeKindCode(Long moId, Long muId, Long areaTypeCode, Long areaTypeKindCode, Integer number, Boolean actual) {
        Specification<Area> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                        moId == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(Area_.moId.getName()), moId),
                        muId == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(Area_.muId.getName()), muId),
                        number == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(Area_.number.getName()), number),
                        areaTypeCode == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(Area_.areaType.getName()), areaTypeCode),
                        areaTypeKindCode == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.notEqual(root.get(Area_.areaType).get(AreaType_.areaTypeKind), areaTypeKindCode),
                        actual == null ? criteriaBuilder.conjunction() :
                                criteriaBuilder.equal(root.get(Area_.archived.getName()), !actual));
        return areaCRUDRepository.findAll(specification);
    }

    @Override
    public List<Area> findDependentAreasByAreaEqAreaType(Area area) {
        Specification<Area> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                    criteriaBuilder.or(
                            criteriaBuilder.equal(root.get(Area_.muId), area.getMuId()),
                            criteriaBuilder.and(
                                    root.get(Area_.moId.getName()).isNull(),
                                    criteriaBuilder.equal(root.get(Area_.moId), area.getMoId())
                            )
                    )
                );
        List<Area> depAreas = areaCRUDRepository.findAll(specification.and(searchActive()));

        return depAreas.stream().filter(da -> !da.getPrimaryAreaTypes().isEmpty() &&
                da.getPrimaryAreaTypes().contains(area.getAreaType())).collect(Collectors.toList());
    }

    @Override
    public List<Area> findPrimaryAreasByAreaEqAreaType(Area area) {
        Specification<Area> specification = (root, criteriaQuery, criteriaBuilder) -> criteriaBuilder.conjunction();
        if (area.getMuId() != null) {
            specification = specification.and(searchByMuIdSpec(area.getMuId()));
        }

        specification = specification.or(searchEmptyMuIdSpec().and(searchByMoIdSpec(area.getMoId())));

        specification = specification.and(searchActive());        

        List<Area> primArea = areaCRUDRepository.findAll(specification);

        return primArea
                .stream()
                .filter(da -> !da.getPrimaryAreaTypes().isEmpty() && da.getPrimaryAreaTypes().stream().map(at -> at.getAreaType()).anyMatch(t -> t.equals(area.getAreaType())))
                .collect(Collectors.toList());
    }

    @Override
    public Long getNextAreaId() {
        return areaSequenceIncrementer.nextLongValue();
    }
}
