package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.area.MoAddress;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MoAddressRepository {

	Page<MoAddress> getActiveMoAddresses(long moId, List<Long> areaTypeCodes, Pageable paging);

	List<MoAddress> getActiveMoAddresses(AreaType areaType);
}
