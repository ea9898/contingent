package moscow.ptnl.contingent.area.transform.v1;

import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

@Mapper(componentModel="spring")
public interface AreaMedicalEmployeesClone {

    AreaMedicalEmployeesClone MAPPER = Mappers.getMapper( AreaMedicalEmployeesClone.class );

    AreaMedicalEmployees clone(AreaMedicalEmployees customerDto);

}
