package moscow.ptnl.contingent.nsi.repository;

import org.springframework.data.repository.NoRepositoryBean;

import java.util.Optional;
import moscow.ptnl.contingent.nsi.domain.area.PositionNom;

@NoRepositoryBean
public interface PositionNomRepository {
    Optional<PositionNom> getByPositionCodeId(Long positionCode);
    Optional<PositionNom> getByPositionCode(String positionCode);
}
