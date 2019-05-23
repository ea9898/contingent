package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.settings.Setting;
import moscow.ptnl.contingent.area.repository.settings.SettingCRUDRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;
import java.util.Set;

@Component
public class SettingServiceImpl implements SettingService {


    @Autowired
    private SettingCRUDRepository settingCRUDRepository;

    private Object getPar(String par) {
        Optional<Setting> settingOptional = settingCRUDRepository.findById(par);
        if (settingOptional.isPresent()) {
            Setting setting = settingOptional.get();
            if (setting.getType().equals(1L)) {
                return setting.getVal();
            }
        }
        return null;
    }

    @Override
    public Long getPar1() {
        return Long.parseLong((String) getPar("PAR_1"));
    }

    @Override
    public Long getPar2() {
        return Long.parseLong((String) getPar("PAR_2"));
    }

    @Override
    public Long getPar3() {
        return Long.parseLong((String) getPar("PAR_3"));
    }
}
