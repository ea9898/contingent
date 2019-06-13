package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeMedicalPositions_;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.List;

@Repository
@Transactional(propagation= Propagation.MANDATORY)
public class AreaTypeMedicalPositionsRepositoryImpl extends BaseRepository implements AreaTypeMedicalPositionsRepository {

    @Autowired
    AreaTypeMedicalPositionsCRUDRepository areaTypeMedicalPositionsCRUDRepository;

    @Override
    public List<AreaTypeMedicalPositions> getPositionsByAreaType(long areaTypeId) {
        Specification<AreaTypeMedicalPositions> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(AreaTypeMedicalPositions_.areaType), areaTypeId);
        return areaTypeMedicalPositionsCRUDRepository.findAll(specification);
    }
}
