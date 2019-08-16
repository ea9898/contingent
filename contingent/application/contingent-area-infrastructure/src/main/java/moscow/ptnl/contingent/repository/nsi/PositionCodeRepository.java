package moscow.ptnl.contingent.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.PositionCode;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.Optional;

@NoRepositoryBean
public interface PositionCodeRepository {
    Optional<PositionCode> getByCode(String code);
}
