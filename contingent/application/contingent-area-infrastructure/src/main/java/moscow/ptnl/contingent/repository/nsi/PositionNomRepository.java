package moscow.ptnl.contingent.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.PositionNom;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.Optional;

@NoRepositoryBean
public interface PositionNomRepository {
    Optional<PositionNom> getByPositionCodeId(Long positionCode);
    Optional<PositionNom> getByPositionCode(String positionCode);
}
