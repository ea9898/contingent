package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.area.MuAddlAreaTypes;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.AreaTypeShort;
import ru.mos.emias.contingent2.core.PagingResults;

@Component
public class SoapCustomMapper {

    public AreaTypeShort mapMuProfileToAreaTypeShort(MuAddlAreaTypes profile) {
        if (profile == null || profile.getAreaType() == null) {
            return null;
        }
        AreaTypeShort areaType = new AreaTypeShort();
        areaType.setCode(profile.getAreaType().getCode());
        areaType.setName(profile.getAreaType().getName());

        return areaType;
    }

    public void mapPagingResults(PagingResults results, Page<?> page) {
        results.setPageNumber(page.getNumber());
        results.setPageSize(page.getSize());
        results.setPageTotal(page.getTotalPages());
        results.setMorePagesAvailable(page.getNumber() < page.getTotalPages() - 1);
    }
}
