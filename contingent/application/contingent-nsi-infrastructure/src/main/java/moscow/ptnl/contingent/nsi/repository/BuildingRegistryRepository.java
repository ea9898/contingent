package moscow.ptnl.contingent.nsi.repository;

import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;
import moscow.ptnl.contingent.nsi.domain.area.NsiBuildingRegistry;

@NoRepositoryBean
public interface BuildingRegistryRepository {

    List<NsiBuildingRegistry> getBuildingsRegistry(long globalId);

    List<NsiBuildingRegistry> findRegistryBuildings(String l1Value, String l2Value, String l3Value, long addrId);
}
