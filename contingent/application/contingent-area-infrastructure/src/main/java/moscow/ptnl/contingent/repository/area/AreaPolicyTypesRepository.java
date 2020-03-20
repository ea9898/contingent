package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.Area;
import org.springframework.data.repository.NoRepositoryBean;
import java.util.List;
import moscow.ptnl.contingent.domain.area.entity.AreaPolicyTypes;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;

@NoRepositoryBean
public interface AreaPolicyTypesRepository {

    List<AreaPolicyTypes> findAll(Area area, PolicyType policyTypeCode);
    
    List<AreaPolicyTypes> findAll(Area area, List<PolicyType> policyTypes);

    void deleteAll(Area area, List<PolicyType> areaPolicyTypesAdd);
}
