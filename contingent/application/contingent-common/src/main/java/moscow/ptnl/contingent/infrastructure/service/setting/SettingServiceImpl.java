package moscow.ptnl.contingent.infrastructure.service.setting;

import moscow.ptnl.contingent.domain.settings.Setting;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import moscow.ptnl.contingent.repository.settings.SettingsRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import sun.rmi.runtime.Log;

@Service
@Transactional(propagation=Propagation.REQUIRED)
public class SettingServiceImpl implements SettingService {
    
    private static final Logger LOG = LoggerFactory.getLogger(SettingServiceImpl.class);
    
    @Autowired
    private SettingsRepository settingsRepository;

    @Override
    public <T> T getSettingProperty(String propertyName) {
        return getSettingProperty(propertyName, false);
    }
    
    @Override
    public <T> T getSettingProperty(String propertyName, boolean refresh) {
        Optional<Setting> settingOptional = settingsRepository.findById(propertyName, refresh);        
        if (settingOptional.isPresent()) {
            Setting setting = settingOptional.get();
            try {                
                return setting.getType().typeValue(setting.getVal());
            } catch (Exception ex) {
                LOG.error(String.format("Ошибка парсинга настройки %s1", propertyName), ex);
            }
        } else {
            LOG.warn("Не найдена настройка {}", propertyName);
        }
        return null;
    }

    @Override
    public Long getPar1() {
        return getSettingProperty(Par1);
    }

    @Override
    public Long getPar2() {
        return getSettingProperty(Par2);
    }

    @Override
    public Long getPar3() {
        return getSettingProperty(Par3);
    }

    @Override
    public List<Long> getPar20() {
        String par20 = getSettingProperty(Par20);
        return Arrays.stream(par20.split(";")).map(Long::parseLong).collect(Collectors.toList());
    }

    @Override
    public Boolean getPar4() {
        Long param = getSettingProperty(Par4);
        return param == null ? null : param.equals(1L);
    }

    @Override
    public Integer getPar5() {
        Long param = getSettingProperty(Par5);
        return param == null ? 0 : param.intValue();
    }

    @Override
    public Integer getPar6() {
        Long param = getSettingProperty(Par6);
        return param == null ? 20 : param.intValue();
    }

    
}
