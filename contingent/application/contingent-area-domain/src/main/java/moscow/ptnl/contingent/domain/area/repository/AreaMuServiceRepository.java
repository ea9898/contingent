package moscow.ptnl.contingent.domain.area.repository;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaMuService;
import org.springframework.data.repository.NoRepositoryBean;

import java.time.LocalDate;
import java.util.List;

@NoRepositoryBean
public interface AreaMuServiceRepository {

    List<AreaMuService> findActive(Long muId, Long excludeAreaId, Long areaTypeProfileCode);

    List<AreaMuService> findActive(Long muId, Long areaId);

    void saveAll(List<AreaMuService> areaMuServices);

    AreaMuService save(AreaMuService areaMuService);

    void closeAreaMuServices(Long servicedMuId, Area area, LocalDate searchEndDate, LocalDate newEndDate);

    AreaMuService findById(Long id);

    List<AreaMuService> findActive(List<Long> areaIds);

    List<AreaMuService> findActiveByMuIds(List<Long> muIds);
}
