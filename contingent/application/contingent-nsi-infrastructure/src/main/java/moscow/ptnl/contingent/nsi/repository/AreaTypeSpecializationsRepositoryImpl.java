package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeSpecializations;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeSpecializations_;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
@Transactional(propagation= Propagation.MANDATORY)
public class AreaTypeSpecializationsRepositoryImpl implements AreaTypeSpecializationsRepository {

    @Autowired
    AreaTypeSpecializationsCRUDRepository areaTypeSpecializationsCRUDRepository;

    @Override
    public List<AreaTypeSpecializations> findBySpecializationCode(Long specializationCode) {
        Specification<AreaTypeSpecializations> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(AreaTypeSpecializations_.specializationCode.getName()), specializationCode);
        return areaTypeSpecializationsCRUDRepository.findAll(specification);
    }

    @Override
    public List<AreaTypeSpecializations> findByAreaTypeCode(AreaType areaType) {
        Specification<AreaTypeSpecializations> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(AreaTypeSpecializations_.areaType.getName()), areaType);
        return areaTypeSpecializationsCRUDRepository.findAll(specification);
    }

    @Override
    public List<AreaTypeSpecializations> findByAreaTypeCode(List<AreaType> areaTypes) {
        Specification<AreaTypeSpecializations> specification = (root, criteriaQuery, criteriaBuilder) ->
                root.get(AreaTypeSpecializations_.areaType.getName()).in(areaTypes);
        return areaTypeSpecializationsCRUDRepository.findAll(specification);
    }
}
