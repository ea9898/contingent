package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.MuAddlAreaTypes;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MuAddlAreaTypesRepository {

	List<MuAddlAreaTypes> getMuAddlAreaTypes(long muId);

    List<MuAddlAreaTypes> findMuAddlAreaTypes(List<Long> muIds, List<Long> areaTypes);
}
