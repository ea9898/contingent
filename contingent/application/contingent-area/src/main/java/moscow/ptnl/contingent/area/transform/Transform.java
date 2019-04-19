package moscow.ptnl.contingent.area.transform;

public interface Transform<DTO, ENTITY> {

    DTO entityToDtoTransform(ENTITY entityObject);

    ENTITY dtoToEntityTransform(DTO dtoObject);

}