package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.settings.Setting;
import moscow.ptnl.contingent.repository.settings.SettingCRUDRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
public class SettingServiceImpl implements SettingService {


    @Autowired
    private SettingCRUDRepository settingCRUDRepository;

    private Object getPar(String par) {
        Optional<Setting> settingOptional = settingCRUDRepository.findById(par);
        if (settingOptional.isPresent()) {
            Setting setting = settingOptional.get();
            if (setting.getType().equals(1L)) {
                return Long.parseLong(setting.getVal());
            }
        }
        return null;
    }

    @Override
    public Long getPar1() {
        return (Long) getPar("max_addresses_for_allocation");
    }

    @Override
    public Long getPar2() {
        return (Long) getPar("max_addresses_for_del_allocation");
    }

    @Override
    public Long getPar3() {
        return (Long) getPar("max_page_size");
    }

    @Override
    public Boolean getPar4() {
        Long param = (Long) getPar("synchronize_k2_and_k1");
        return param == null ? null : param.equals(1L);
    }
}
