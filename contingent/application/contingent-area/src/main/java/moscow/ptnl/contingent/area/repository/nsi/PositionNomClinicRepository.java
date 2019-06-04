package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.PositionNom;
import org.springframework.data.repository.NoRepositoryBean;

@NoRepositoryBean
public interface PositionNomClinicRepository {
    PositionNom getPositionProxy(long positionId);
}
