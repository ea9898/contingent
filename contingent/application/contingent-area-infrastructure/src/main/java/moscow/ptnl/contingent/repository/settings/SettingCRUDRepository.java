package moscow.ptnl.contingent.repository.settings;

import moscow.ptnl.contingent.area.entity.settings.Setting;
import moscow.ptnl.contingent.repository.CommonRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface SettingCRUDRepository extends CommonRepository<Setting, String> {
    
}
