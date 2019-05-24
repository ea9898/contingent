package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.BuildingRegistry;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface RegistryBuildingRepository {

    List<BuildingRegistry> getRegistryBuildings(long globalId);

    List<BuildingRegistry> findRegistryBuildings(String l1Value, String l2Value, String l3Value, long addrId);
}
