package moscow.ptnl.contingent.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.PositionCode;
import moscow.ptnl.contingent.area.entity.nsi.PositionCode_;
import moscow.ptnl.contingent.area.entity.nsi.PositionNom;
import moscow.ptnl.contingent.area.entity.nsi.PositionNom_;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import java.util.List;
import java.util.Optional;

@Repository
@Transactional(propagation = Propagation.MANDATORY)
public class PositionNomRepositoryImpl extends BaseRepository implements PositionNomRepository {

    @Autowired
    PositionNomCRUDRepository positionNomCRUDRepository;

    @Override
    public PositionNom getPositionProxy(long positionId) {
        return entityManager.getReference(PositionNom.class, positionId);
    }

    @Override
    public List<PositionNom> searchPostitionNomActualByCode(Long positionId) {
        Specification<PositionNom> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                        criteriaBuilder.equal(root.get(PositionNom_.globalId.getName()), positionId)
//                    criteriaBuilder.equal(root.get(PositionNom_.archived.getName()), false) CONTINGENT2-280
                );
        return positionNomCRUDRepository.findAll(specification);
    }

    @Override
    public Optional<PositionNom> getByCode(String code) {
        Specification<PositionNom> specification = (root, query, builder) -> {
            final Join<PositionNom, PositionCode> positionCodeJoin = root.join(PositionNom_.positionCode, JoinType.LEFT);
            return builder.equal(positionCodeJoin.get(PositionCode_.code), code);
        };
        return positionNomCRUDRepository.findOne(specification);
    }
}
