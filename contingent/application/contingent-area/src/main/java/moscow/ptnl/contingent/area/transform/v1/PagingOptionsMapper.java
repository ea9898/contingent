package moscow.ptnl.contingent.area.transform.v1;

import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.transform.Transform;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.PagingOptions;

@Component
@Deprecated //Использовать SoapCustomMapper.mapPagingOptions
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
