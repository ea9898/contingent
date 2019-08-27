package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeSpecializations;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface AreaTypeSpecializationsRepository {
    List<AreaTypeSpecializations> findBySpecializationCode(Long specializationCode);

    List<AreaTypeSpecializations> findByAreaTypeCode(AreaType areaType);

    List<AreaTypeSpecializations> findByAreaTypeCode(List<AreaType> areaTypes);
}
