package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.domain.area.entity.Area;

import java.util.List;

public interface EsuHelperService {

    void sendAreaInfoEvent(Area area, String methodName);

    void sendAttachOnAreaChangeEvent(
            List<Long> primaryAreasIdCreateAttachments,
            List<Long> primaryAreasIdCloseAttachments,
            Area dependentArea);
}
