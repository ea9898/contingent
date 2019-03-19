package moscow.ptnl.contingent.repository.settings;

import java.io.Serializable;
import java.util.List;

import org.springframework.data.repository.NoRepositoryBean;

@NoRepositoryBean
public interface SettingRepository<Setting, ID extends Serializable> {

	List<Setting> getSettingByRelease(String value);
	
	Setting getSettingsByKey(String key);

}
