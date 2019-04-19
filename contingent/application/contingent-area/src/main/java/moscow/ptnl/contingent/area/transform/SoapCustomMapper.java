package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.area.MuProfile;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Component;
import ru.gov.emias2.contingent.v1.area.types.AreaTypeShort;
import ru.gov.emias2.contingent.v1.common.types.PagingResult;

@Component
public class SoapCustomMapper {

    public AreaTypeShort mapMuProfileToAreaTypeShort(MuProfile profile) {
        if (profile == null || profile.getAreaType() == null) {
            return null;
        }
        AreaTypeShort areaType = new AreaTypeShort();
        areaType.setCode(profile.getAreaType().getCode());
        areaType.setName(profile.getAreaType().getName());

        return areaType;
    }

    public PagingResult mapPageToPagingResult(Page<?> page) {
        PagingResult pagingResult = new PagingResult();
        pagingResult.setPageNumber(page.getNumber());
        pagingResult.setPageSize(page.getSize());
        pagingResult.setPageTotal(page.getTotalPages());
        pagingResult.setMorePagesAvailable(page.getNumber() < page.getTotalPages() - 1);

        return pagingResult;
    }
}
