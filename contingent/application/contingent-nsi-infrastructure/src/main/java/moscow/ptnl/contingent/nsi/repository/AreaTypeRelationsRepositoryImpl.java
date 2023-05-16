package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeRelationsRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeRelations;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeRelations_;

@Repository
@Transactional(propagation = Propagation.MANDATORY)
public class AreaTypeRelationsRepositoryImpl implements AreaTypeRelationsRepository {

    @Autowired
    AreaTypeRelationsCRUDRepository areaTypeRelationsCRUDRepository;

    @Override
    public Optional<AreaTypeRelations> getByDependentAndPrimaryAreaTypes(AreaType dependentAreaType, AreaType primaryAreaType) {
        Specification<AreaTypeRelations> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                        criteriaBuilder.equal(root.get(AreaTypeRelations_.dependentAreaType), dependentAreaType),
                        criteriaBuilder.equal(root.get(AreaTypeRelations_.primaryAreaType), primaryAreaType),
                        criteriaBuilder.notEqual(root.get(AreaTypeRelations_.archived), true)
                );

        return areaTypeRelationsCRUDRepository.findOne(specification);
    }
}
