package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.MuAddlAreaTypes;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MuProfileRepository {

	List<MuAddlAreaTypes> getMuProfilesByMuId(long muId);

    List<MuAddlAreaTypes> findMuProfilesByMuIdAndAreaTypes(long muId, List<Long> areaTypes);
}
