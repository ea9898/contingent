package moscow.ptnl.contingent.nsi.domain.repository;

import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;
import java.util.Optional;
import java.util.Set;

import moscow.ptnl.contingent.nsi.domain.area.PositionCode;

@NoRepositoryBean
public interface PositionCodeRepository {
    Optional<PositionCode> getByCode(String code);

    List<PositionCode> getByGlobalIds(Set<Long> globalIds);
}
