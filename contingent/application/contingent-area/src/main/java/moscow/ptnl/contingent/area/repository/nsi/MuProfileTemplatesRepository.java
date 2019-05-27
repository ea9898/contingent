package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.MUTypeAreaTypes;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MuProfileTemplatesRepository {

	MUTypeAreaTypes findMuProfileTemplate(int muTypeId, Long areaTypeCode);

    List<MUTypeAreaTypes> findMuProfileTemplates(Long muTypeId, List<Long> areaTypeCodes);
}
