package moscow.ptnl.contingent.transform;

public interface Transform<DTO, ENTITY> {

    DTO entityToDtoTransform(ENTITY entityObject);

    ENTITY dtoToEntityTransform(DTO dtoObject);

}
