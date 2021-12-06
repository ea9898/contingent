package moscow.ptnl.contingent.area.transform.v2;

import moscow.ptnl.contingent.nsi.domain.area.CodeName;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.v2.CodeNameType;

@Component
public class CodeNameTypeMapperV2 {

    public <T extends CodeNameType, V extends CodeName> T entityToDtoTransform(V entityObject, Class<T> targetClass) {
        if (entityObject == null) {
            return null;
        }
        try {
            T dtoObject = targetClass.newInstance();
            dtoObject.setCode(entityObject.getCode());
            dtoObject.setName(entityObject.getTitle());

            return dtoObject;
        }
        catch (InstantiationException | IllegalAccessException e) {
            throw new IllegalStateException("Can't instantiate class " + targetClass.getName());
        }
    }
}
