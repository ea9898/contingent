package moscow.ptnl.contingent.area.transform.v2;

import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import moscow.ptnl.contingent.transform.Transform;
import org.springframework.stereotype.Component;
import ru.lanit.emias.util.LocalDateConvertUtils;
import ru.lanit.emias.util.LocalDateTimeConvertUtils;
import ru.mos.emias.contingent2.core.v2.AreaDn;
import ru.mos.emias.contingent2.core.v2.AreaHistory;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Component
public class GetAreaHistoryMapperV2 implements Transform<AreaHistory.Events.Event, moscow.ptnl.contingent.domain.area.model.area.AreaHistory.Event> {

    @Override
    public AreaHistory.Events.Event entityToDtoTransform(moscow.ptnl.contingent.domain.area.model.area.AreaHistory.Event entityObject) {
        AreaHistory.Events.Event event = new AreaHistory.Events.Event();
        event.setJobId(entityObject.getJobId().longValue());
        event.setSnils(entityObject.getSnils());
        event.setLogin(entityObject.getLogin());
        event.setUpdateDate(entityObject.getUpdateDate());
        event.setIsReplacement(BigDecimal.valueOf(1).equals(entityObject.getIsReplacement()));
        if (entityObject.getStartDate() != null) {
            event.setStartDate(LocalDateTimeConvertUtils.parse(entityObject.getStartDate()));
        }
        if (entityObject.getEndDate() != null) {
            event.setEndDate(LocalDateTimeConvertUtils.parse(entityObject.getEndDate()));
        }
        if (entityObject.getIsError() != null) {
            event.setIsError("true".equals(entityObject.getIsError()));
        }
        return event;
    }

    @Override
    public moscow.ptnl.contingent.domain.area.model.area.AreaHistory.Event dtoToEntityTransform(AreaHistory.Events.Event dtoObject) {
        return null;
    }

}
