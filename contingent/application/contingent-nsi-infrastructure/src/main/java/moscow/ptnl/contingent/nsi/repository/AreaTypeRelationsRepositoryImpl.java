package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeRelations;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeRelations_;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

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
                        criteriaBuilder.isFalse(root.get(AreaTypeRelations_.archived))
                );

        return areaTypeRelationsCRUDRepository.findOne(specification);
    }
}
