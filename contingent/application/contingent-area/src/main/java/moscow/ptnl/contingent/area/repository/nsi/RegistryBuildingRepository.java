package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.RegistryBuilding;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface RegistryBuildingRepository {

    List<RegistryBuilding> getRegistryBuildings(long globalId);

    List<RegistryBuilding> findRegistryBuildings(String l1Value, String l2Value, String l3Value, long addrId);
}
