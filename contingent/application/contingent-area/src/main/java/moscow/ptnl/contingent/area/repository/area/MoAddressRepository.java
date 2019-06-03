package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MoAddressRepository {

	Page<MoAddress> getActiveMoAddresses(long moId, List<Long> areaTypeCodes, Pageable paging);

	List<MoAddress> getActiveMoAddresses(AreaType areaType);
}
