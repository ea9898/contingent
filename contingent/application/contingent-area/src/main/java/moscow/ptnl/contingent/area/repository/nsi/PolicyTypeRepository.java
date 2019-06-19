package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.MuTypeAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.PolicyType;
import moscow.ptnl.contingent.area.model.nsi.AvailableToCreateType;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface PolicyTypeRepository {

	List<PolicyType> findByIds(List<Long> ids);
}
