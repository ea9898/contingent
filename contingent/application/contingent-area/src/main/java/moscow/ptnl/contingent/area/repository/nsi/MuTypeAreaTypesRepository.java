package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.MuTypeAreaTypes;
import moscow.ptnl.contingent.area.model.nsi.AvailableToCreateType;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MuTypeAreaTypesRepository {

	MuTypeAreaTypes findMuProfileTemplate(int muTypeId, Long areaTypeCode);

    List<MuTypeAreaTypes> findMuTypeAreaTypes(Long muTypeId, List<Long> areaTypeCodes, AvailableToCreateType availableToCreate);

    List<MuTypeAreaTypes> findMuTypeAreaTypes(List<Long> muTypes, List<Long> areaTypeCodes, AvailableToCreateType availableToCreate);

    List<MuTypeAreaTypes> findMuTypeAreaTypes(Long muTypeId, List<Long> areaTypeCodes);
}
