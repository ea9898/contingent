package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.nsi.AreaPolicyTypes;
import moscow.ptnl.contingent.area.entity.nsi.PolicyType;
import org.springframework.data.repository.NoRepositoryBean;
import java.util.List;

@NoRepositoryBean
public interface AreaPolicyTypesRepository {

    List<AreaPolicyTypes> findAll(Area area, PolicyType policyTypeCode);

    void deleteAll(Area area, List<PolicyType> areaPolicyTypesAdd);
}
