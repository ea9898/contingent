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
    public Optional<PositionNom> getByPositionCodeId(Long positionCode) {
        Specification<PositionNom> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                        criteriaBuilder.equal(root.get(PositionNom_.positionCodeId.getName()), positionCode)
                );
        return positionNomCRUDRepository.findOne(specification);
    }
}
