package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.area.MuProfile;
import org.springframework.stereotype.Component;
import ru.gov.emias2.contingent.v1.area.types.AreaTypeShort;

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
}
