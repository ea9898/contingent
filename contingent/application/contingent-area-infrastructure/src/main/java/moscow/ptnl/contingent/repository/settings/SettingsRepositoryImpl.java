/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.repository.settings;

import java.util.Optional;
import moscow.ptnl.contingent.area.entity.settings.Setting;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 * @author m.kachalov
 */
@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class SettingsRepositoryImpl extends BaseRepository implements SettingsRepository {
    
    @Override
    public Optional<Setting> findById(String propertyName, boolean refresh) {
        Setting result = getEntityManager().find(Setting.class, propertyName);
        if (refresh && result != null) {
            getEntityManager().refresh(result);            
        }
        return (result != null) ? Optional.of(result) : Optional.empty();
    }
    
}
