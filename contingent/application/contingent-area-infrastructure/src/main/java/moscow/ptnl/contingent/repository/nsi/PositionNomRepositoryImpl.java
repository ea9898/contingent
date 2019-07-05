package moscow.ptnl.contingent.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.PositionNom;
import moscow.ptnl.contingent.area.entity.nsi.PositionNom_;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Repository
@Transactional(propagation= Propagation.MANDATORY)
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
                    criteriaBuilder.equal(root.get(PositionNom_.id.getName()), positionId),
                    criteriaBuilder.equal(root.get(PositionNom_.archived.getName()), false)
            );
        return positionNomCRUDRepository.findAll(specification);
    }

    @Override
    public Optional<PositionNom> getByCode(String code) {
        Specification<PositionNom> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                        criteriaBuilder.equal(root.get(PositionNom_.code.getName()), code)
                );
        return positionNomCRUDRepository.findOne(specification);
    }
}
