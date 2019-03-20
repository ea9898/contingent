package moscow.ptnl.contingent.area.repository.settings;

import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import moscow.ptnl.contingent.area.entity.settings.Setting;
import moscow.ptnl.contingent.area.repository.CommonRepository;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface SettingCRUDRepository extends CommonRepository<Setting, Long>{

}
