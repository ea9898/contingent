package moscow.ptnl.contingent.area.transform;

import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.PagingOptions;

@Component
public class PagingOptionsMapper implements Transform<PagingOptions, PageRequest> {

    @Override
    public PagingOptions entityToDtoTransform(PageRequest entityObject) {
        return null;
    }

    @Override
    public PageRequest dtoToEntityTransform(PagingOptions dtoObject) {
        return PageRequest.of(dtoObject.getPageNumber(), dtoObject.getPageSize());
    }
}
