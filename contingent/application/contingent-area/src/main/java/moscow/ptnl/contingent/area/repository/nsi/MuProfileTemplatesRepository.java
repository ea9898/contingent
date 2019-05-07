package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.MUProfileTemplates;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MuProfileTemplatesRepository {

	MUProfileTemplates findMuProfileTemplate(int muTypeId, Long areaTypeCode);

    List<MUProfileTemplates> findMuProfileTemplates(Long muTypeId, List<Long> areaTypeCodes);
}
