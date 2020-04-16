package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Repository
@Transactional(propagation= Propagation.MANDATORY)
public class AreaTypesRepositoryImpl extends BaseRepository implements moscow.ptnl.contingent.nsi.domain.repository.AreaTypesRepository {

    @Override
    public Optional<AreaType> findById(Long areaTypeId) {
        return Optional.empty();
    }
}
