package moscow.ptnl.contingent.nsi.repository;

import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;

@NoRepositoryBean
public interface PolicyTypeRepository {

	List<PolicyType> findByIds(List<Long> ids);
}
