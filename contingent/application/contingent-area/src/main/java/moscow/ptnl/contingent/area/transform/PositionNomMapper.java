package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.nsi.domain.area.PositionNom;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.PositionNomClinic;

@Component

public class PositionNomMapper implements Transform<PositionNomClinic, PositionNom> {

    @Override
    public PositionNomClinic entityToDtoTransform(PositionNom entityObject) {
        PositionNomClinic employee = new PositionNomClinic();
//        employee.setCode(entityObject.getCode()); CONTINGENT2-280
        employee.setName(entityObject.getTitle());

        return employee;
    }

    @Override
    public PositionNom dtoToEntityTransform(PositionNomClinic dtoObject) {
        return null;
    }
}
