package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.AreaPolicyTypes;
import org.springframework.data.repository.NoRepositoryBean;
import java.util.List;

@NoRepositoryBean
public interface AreaPolicyTypesRepository {

    List<AreaPolicyTypes> findAll(long areaId, long policyTypeCode);

    void deleteAll(long areaId, List<Long> areaPolicyTypesAdd);
}
