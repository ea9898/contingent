package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.repository.PositionCodeRepository;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import moscow.ptnl.contingent.nsi.domain.area.PositionCode;
import moscow.ptnl.contingent.nsi.domain.area.PositionCode_;

@Repository
@Transactional(propagation= Propagation.MANDATORY)
public class PositionCodeRepositoryImpl extends BaseRepository implements PositionCodeRepository {

    @Autowired
    PositionCodeCRUDRepository positionCodeCRUDRepository;

    @Override
    public Optional<PositionCode> getByCode(String code) {
        Specification<PositionCode> specification = (root, query, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(PositionCode_.code.getName()), code);
        return positionCodeCRUDRepository.findOne(specification);
    }
}
