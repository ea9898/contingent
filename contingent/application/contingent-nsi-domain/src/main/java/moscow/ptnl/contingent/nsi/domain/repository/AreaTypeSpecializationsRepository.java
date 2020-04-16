package moscow.ptnl.contingent.nsi.domain.repository;

import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeSpecializations;

@NoRepositoryBean
public interface AreaTypeSpecializationsRepository {
    
    List<AreaTypeSpecializations> findBySpecializationCode(Long specializationCode);

    List<AreaTypeSpecializations> findByAreaTypeCode(AreaType areaType);

    List<AreaTypeSpecializations> findByAreaTypeCode(List<AreaType> areaTypes);
}
