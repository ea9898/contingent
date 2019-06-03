package moscow.ptnl.contingent.area.transform;

import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.PositionNomClinic;

@Component

public class PositionNomClinicMapper implements Transform<PositionNomClinic, moscow.ptnl.contingent.area.entity.nsi.PositionNomClinic> {

    @Override
    public PositionNomClinic entityToDtoTransform(moscow.ptnl.contingent.area.entity.nsi.PositionNomClinic entityObject) {
        PositionNomClinic employee = new PositionNomClinic();
        employee.setCode(entityObject.getId());
        employee.setName(entityObject.getTitle());

        return employee;
    }

    @Override
    public moscow.ptnl.contingent.area.entity.nsi.PositionNomClinic dtoToEntityTransform(PositionNomClinic dtoObject) {
        return null;
    }
}
