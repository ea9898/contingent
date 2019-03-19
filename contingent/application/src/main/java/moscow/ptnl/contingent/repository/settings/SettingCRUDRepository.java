package moscow.ptnl.contingent.repository.settings;

import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import moscow.ptnl.contingent.entity.settings.Setting;
import moscow.ptnl.contingent.repository.CommonRepository;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface SettingCRUDRepository extends CommonRepository<Setting, Long>{

}
