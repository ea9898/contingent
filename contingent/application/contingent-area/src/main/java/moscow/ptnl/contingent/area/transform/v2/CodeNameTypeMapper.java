package moscow.ptnl.contingent.area.transform.v2;

import moscow.ptnl.contingent.nsi.domain.area.CodeName;
import ru.mos.emias.contingent2.core.v2.CodeNameType;

public class CodeNameTypeMapper<T extends CodeNameType, V extends CodeName> {

    private T dtoObject;

    private V entityObject;

    public CodeNameTypeMapper(T dtoObject, V entityObject) {
        this.dtoObject = dtoObject;
        this.entityObject = entityObject;
    }

    public T entityToDtoTransform() {
        dtoObject.setCode(entityObject.getCode());
        dtoObject.setName(entityObject.getTitle());
        return dtoObject;
    }

    public V dtoToEntityTransform() {
        entityObject.setCode(dtoObject.getCode());
        entityObject.setTitle(dtoObject.getName());
        return entityObject;
    }
}
