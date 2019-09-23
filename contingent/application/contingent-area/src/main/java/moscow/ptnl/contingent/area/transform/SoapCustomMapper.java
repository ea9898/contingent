package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.area.MuAddlAreaTypes;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.AreaTypeShort;
import ru.mos.emias.contingent2.core.PagingResults;

@Component
public class SoapCustomMapper {

    public void mapPagingResults(PagingResults results, Page<?> page) {
        results.setPageNumber(page.getNumber());
        results.setPageSize(page.getSize());
        results.setPageTotal(page.getTotalPages() > 0 ? page.getTotalPages() - 1 : 0);
        results.setMorePagesAvailable(page.getNumber() < page.getTotalPages() - 1);
    }
}
