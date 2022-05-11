package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.MuMuService;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.repository.PagingAndSortingRepository;

import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface MuMuServiceCRUDRepository extends PagingAndSortingRepository<MuMuService, Long> {

    List<MuMuService> findByMuIdAndAreaType(long muId, AreaType areaType);

    List<MuMuService> findByServiceMuIdAndAreaType(long serviceMuId, AreaType areaType);
}
