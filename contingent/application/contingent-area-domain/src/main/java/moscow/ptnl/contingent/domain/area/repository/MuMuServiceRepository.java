package moscow.ptnl.contingent.domain.area.repository;

import moscow.ptnl.contingent.domain.area.entity.MuMuService;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MuMuServiceRepository {

    List<MuMuService> getMuMuServices(long muId, AreaType areaType);

    List<MuMuService> getServicingMU(long serviceMuId, AreaType areaType);
}
