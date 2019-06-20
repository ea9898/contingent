package moscow.ptnl.contingent.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.PositionNom;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface PositionNomRepository {
    PositionNom getPositionProxy(long positionId);

    List<PositionNom> searchPostitionNomActualByCode(Long positionId);
}
