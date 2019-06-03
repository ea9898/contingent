package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.PositionNomClinic;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation= Propagation.MANDATORY)
public class PositionNomClinicRepositoryImpl extends BaseRepository implements PositionNomClinicRepository {

    @Override
    public PositionNomClinic getPositionProxy(long positionId) {
        return entityManager.getReference(PositionNomClinic.class, positionId);
    }
}
