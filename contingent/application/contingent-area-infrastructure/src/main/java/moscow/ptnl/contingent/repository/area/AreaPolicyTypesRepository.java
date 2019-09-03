package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Area;
import org.springframework.data.repository.NoRepositoryBean;
import java.util.List;
import moscow.ptnl.contingent.area.entity.area.AreaPolicyTypes;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;

@NoRepositoryBean
public interface AreaPolicyTypesRepository {

    List<AreaPolicyTypes> findAll(Area area, PolicyType policyTypeCode);

    void deleteAll(Area area, List<PolicyType> areaPolicyTypesAdd);
}
