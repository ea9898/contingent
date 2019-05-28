package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.MuTypeAreaTypes;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MuTypeAreaTypesRepository {

	MuTypeAreaTypes findMuProfileTemplate(int muTypeId, Long areaTypeCode);

    List<MuTypeAreaTypes> findMuProfileTemplates(Long muTypeId, List<Long> areaTypeCodes, Boolean availableToCreate);

    List<MuTypeAreaTypes> findMuTypeAreaTypes(Long muTypeId, List<Long> areaTypeCodes);
}
