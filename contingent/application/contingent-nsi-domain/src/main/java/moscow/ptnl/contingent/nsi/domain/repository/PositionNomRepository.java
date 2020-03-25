package moscow.ptnl.contingent.nsi.domain.repository;

import org.springframework.data.repository.NoRepositoryBean;

import java.util.Optional;
import moscow.ptnl.contingent.nsi.domain.area.PositionNom;

@NoRepositoryBean
public interface PositionNomRepository {

    Optional<PositionNom> getByPositionCodeId(Long positionCodeId);

    Optional<PositionNom> getByPositionCode(String positionCode);

}
