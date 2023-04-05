package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.area.AreaType_;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeMedicalPositionsRepository;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeMedicalPositions_;

@Repository
@Transactional(propagation= Propagation.MANDATORY)
public class AreaTypeMedicalPositionsRepositoryImpl extends BaseRepository implements AreaTypeMedicalPositionsRepository {

    @Autowired
    AreaTypeMedicalPositionsCRUDRepository areaTypeMedicalPositionsCRUDRepository;

    @Override
    public List<AreaTypeMedicalPositions> getPositionsByAreaType(long areaTypeId) {
        Specification<AreaTypeMedicalPositions> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(AreaTypeMedicalPositions_.areaType).get(AreaType_.code), areaTypeId);
        return areaTypeMedicalPositionsCRUDRepository.findAll(specification);
    }
}
