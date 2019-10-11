package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.service.setting.SettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.PagingOptions;

@Component
public class PagingOptionsMapper implements Transform<PagingOptions, PageRequest> {

    @Autowired
    private SettingService settingService;

    @Override
    public PagingOptions entityToDtoTransform(PageRequest entityObject) {
        return null;
    }

    @Override
    public PageRequest dtoToEntityTransform(PagingOptions dtoObject) {
        if (dtoObject == null) {
            return PageRequest.of(settingService.getPar5(), settingService.getPar6());
        }
        return PageRequest.of(dtoObject.getPageNumber(), dtoObject.getPageSize());
    }
}
