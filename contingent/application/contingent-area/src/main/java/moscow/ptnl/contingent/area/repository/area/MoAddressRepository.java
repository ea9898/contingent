package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.MoAddress;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MoAddressRepository {

	List<MoAddress> getActiveMoAddresses(long moId, long areaTypeCode);
}
