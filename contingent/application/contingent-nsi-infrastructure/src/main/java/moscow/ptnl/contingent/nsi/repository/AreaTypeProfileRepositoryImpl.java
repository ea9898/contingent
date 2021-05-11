package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.area.AreaTypeProfile;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Repository
@Transactional(propagation= Propagation.MANDATORY)
public class AreaTypeProfileRepositoryImpl extends BaseRepository implements moscow.ptnl.contingent.nsi.domain.repository.AreaTypeProfileRepository {

    @Autowired
    private AreaTypeProfileCRUDRepository areaTypeProfileCRUDRepository;

    @Override
    public Optional<AreaTypeProfile> findByCodeAndAreaType(Long areaTypeProfileCode, Long areaTypeCode) {
        return areaTypeProfileCRUDRepository.findByCodeAndAreaTypeCode(areaTypeProfileCode, areaTypeCode);
    }
}
