/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.repository.settings;


import java.util.Optional;
import moscow.ptnl.contingent.area.entity.settings.Setting;
import org.springframework.data.repository.NoRepositoryBean;

/**
 *
 * @author m.kachalov
 */
@NoRepositoryBean
public interface SettingsRepository {
    
    Optional<Setting> findById(String propertyName, boolean refresh);
    
}
