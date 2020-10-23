package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeSpecializationsRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeSpecializations;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeSpecializations_;
import moscow.ptnl.contingent.nsi.domain.area.AreaType_;

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
        List<Long> areaTypesIds = areaTypes.stream().map(a -> a.getCode()).collect(Collectors.toList());
        Specification<AreaTypeSpecializations> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.in(root.get(AreaTypeSpecializations_.areaType.getName()).get(AreaType_.code.getName())).value(areaTypesIds); //root.get(AreaTypeSpecializations_.areaType.getName()).in(areaTypes);
        return areaTypeSpecializationsCRUDRepository.findAll(specification);
    }
}
