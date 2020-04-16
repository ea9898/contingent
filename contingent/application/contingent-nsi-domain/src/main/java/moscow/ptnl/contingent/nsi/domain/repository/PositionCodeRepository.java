package moscow.ptnl.contingent.nsi.domain.repository;

import org.springframework.data.repository.NoRepositoryBean;

import java.util.Optional;
import moscow.ptnl.contingent.nsi.domain.area.PositionCode;

@NoRepositoryBean
public interface PositionCodeRepository {
    Optional<PositionCode> getByCode(String code);
}
