package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.area.PositionCode;
import moscow.ptnl.contingent.nsi.domain.area.PositionCode_;
import moscow.ptnl.contingent.nsi.domain.area.PositionNom;
import moscow.ptnl.contingent.nsi.domain.area.PositionNom_;
import moscow.ptnl.contingent.nsi.domain.repository.PositionNomRepository;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Repository
@Transactional(propagation = Propagation.MANDATORY)
public class PositionNomRepositoryImpl extends BaseRepository implements PositionNomRepository {

    @Autowired
    PositionNomCRUDRepository positionNomCRUDRepository;

    @Override
    public Optional<PositionNom> getByPositionCodeId(Long positionCodeId) {
        Specification<PositionNom> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(PositionNom_.positionCodeId), positionCodeId);
        return positionNomCRUDRepository.findOne(specification);
    }

    @Override
    public List<PositionNom> findByPositionCodeIds(List<Long> positionCodeId) {
        Specification<PositionNom> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.in(root.get(PositionNom_.positionCodeId.getName())).value(positionCodeId);
        return positionNomCRUDRepository.findAll(specification);
    }
}
