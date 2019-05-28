package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.MUTypeAreaTypes;
import moscow.ptnl.contingent.area.model.nsi.AvailableToCreateType;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MUTypeAreaTypesRepository {

	MUTypeAreaTypes findMuProfileTemplate(int muTypeId, Long areaTypeCode);

    List<MUTypeAreaTypes> findMuTypeAreaTypes(Long muTypeId, List<Long> areaTypeCodes, AvailableToCreateType availableToCreate);

    List<MUTypeAreaTypes> findMuTypeAreaTypes(List<Long> muTypes, List<Long> areaTypeCodes, AvailableToCreateType availableToCreate);
}
