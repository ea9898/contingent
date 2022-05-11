package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.MuMuService;
import moscow.ptnl.contingent.domain.area.repository.MuMuServiceRepository;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.repository.BaseRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class MuMuServiceRepositoryImpl extends BaseRepository implements MuMuServiceRepository {

    @Autowired
    private MuMuServiceCRUDRepository muMuServiceCRUDRepository;

    @Override
    public List<MuMuService> getMuMuServices(long muId, AreaType areaType) {
        return muMuServiceCRUDRepository.findByMuIdAndAreaType(muId, areaType);
    }

    @Override
    public List<MuMuService> getServicingMU(long serviceMuId, AreaType areaType) {
        return muMuServiceCRUDRepository.findByServiceMuIdAndAreaType(serviceMuId, areaType);
    }
}
