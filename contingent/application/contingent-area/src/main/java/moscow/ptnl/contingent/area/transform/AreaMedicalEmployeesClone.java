package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

@Mapper(componentModel="spring")
public interface AreaMedicalEmployeesClone {

    AreaMedicalEmployeesClone MAPPER = Mappers.getMapper( AreaMedicalEmployeesClone.class );

    AreaMedicalEmployees clone(AreaMedicalEmployees customerDto);

}
