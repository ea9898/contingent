package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.area.MappingPositionCodeToOtherPosition;
import moscow.ptnl.contingent.nsi.domain.area.PositionSupp;
import moscow.ptnl.contingent.nsi.domain.repository.MappingPositionCodeToOtherPositionRepository;
import moscow.ptnl.contingent.repository.BaseRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class MappingPositionCodeToOtherPositionRepositoryImpl extends BaseRepository implements MappingPositionCodeToOtherPositionRepository {

    @Autowired
    PositionSuppCRUDRepository positionSuppCRUDRepository;

    @Autowired
    MappingPositionCodeToOtherPositionCRUDRepository mappingPositionCodeToOtherPositionCRUDRepository;

    @Override
    public List<MappingPositionCodeToOtherPosition> findByPositionSuppCode(String suppCode) {
        PositionSupp positionSupp = positionSuppCRUDRepository.findByCode(suppCode).orElse(null);
        return positionSupp == null ? Collections.emptyList() :
                mappingPositionCodeToOtherPositionCRUDRepository.findByPsGlobalId(positionSupp.getGlobalId());
    }
}
