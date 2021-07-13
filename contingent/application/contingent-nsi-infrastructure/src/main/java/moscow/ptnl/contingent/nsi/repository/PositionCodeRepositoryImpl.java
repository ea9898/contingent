package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.repository.PositionCodeRepository;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import moscow.ptnl.contingent.nsi.domain.area.PositionCode;

@Repository
@Transactional(propagation= Propagation.MANDATORY)
public class PositionCodeRepositoryImpl extends BaseRepository implements PositionCodeRepository {

    @Autowired
    PositionCodeCRUDRepository positionCodeCRUDRepository;

    @Override
    public Optional<PositionCode> getByCode(String code) {
        return positionCodeCRUDRepository.findById(code);
    }
}
