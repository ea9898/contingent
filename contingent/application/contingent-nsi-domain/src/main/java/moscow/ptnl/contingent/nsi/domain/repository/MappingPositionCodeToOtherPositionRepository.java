package moscow.ptnl.contingent.nsi.domain.repository;

import moscow.ptnl.contingent.nsi.domain.area.MappingPositionCodeToOtherPosition;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MappingPositionCodeToOtherPositionRepository {

    List<MappingPositionCodeToOtherPosition> findByPositionSuppCode(String suppCode);
}
