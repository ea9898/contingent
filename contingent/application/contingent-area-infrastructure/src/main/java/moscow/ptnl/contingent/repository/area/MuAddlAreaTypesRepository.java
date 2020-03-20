package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.MuAddlAreaTypes;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MuAddlAreaTypesRepository {

	List<MuAddlAreaTypes> getMuAddlAreaTypes(long muId);

    List<MuAddlAreaTypes> findMuAddlAreaTypes(List<Long> muIds, List<Long> areaTypes);
}
