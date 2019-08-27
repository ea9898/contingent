package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.area.entity.nsi.NsiBuildingRegistry;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface BuildingRegistryRepository {

    List<NsiBuildingRegistry> getBuildingsRegistry(long globalId);

    List<NsiBuildingRegistry> findRegistryBuildings(String l1Value, String l2Value, String l3Value, long addrId);
}
