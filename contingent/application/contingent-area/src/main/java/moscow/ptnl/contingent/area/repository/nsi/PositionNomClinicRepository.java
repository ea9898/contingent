package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.PositionNomClinic;
import org.springframework.data.repository.NoRepositoryBean;

@NoRepositoryBean
public interface PositionNomClinicRepository {
    PositionNomClinic getPositionProxy(long positionId);
}
