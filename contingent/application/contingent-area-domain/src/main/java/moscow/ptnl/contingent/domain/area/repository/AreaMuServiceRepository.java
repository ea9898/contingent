package moscow.ptnl.contingent.domain.area.repository;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaMuService;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AreaMuServiceRepository {

    List<AreaMuService> findActive(Long muId, Long excludeAreaId, Long areaTypeProfileCode);

    List<AreaMuService> findActive(Long muId, Long areaId);

    void saveAll(List<AreaMuService> areaMuServices);

    void closeAreaMuServices(Long servicedMuId, Area area);
}
