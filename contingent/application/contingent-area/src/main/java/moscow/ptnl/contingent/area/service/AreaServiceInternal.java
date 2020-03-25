package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.domain.area.model.area.AddMedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import moscow.ptnl.contingent.domain.area.model.area.SearchAreaAddress;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import ru.mos.emias.contingent2.address.AddressRegistryBaseType;
import ru.mos.emias.contingent2.area.types.SearchAreaRequest;


import java.util.List;

public interface AreaServiceInternal {

    Long initiateCreatePrimaryArea(long moId, Long muId, Integer number, String description, Long areaTypeCode,
                                   List<Long> policyTypes, Integer ageMin, Integer ageMax, Integer ageMinM,
                                   Integer ageMaxM, Integer ageMinW, Integer ageMaxW, boolean autoAssignForAttachment,
                                   Boolean attachByMedicalReason, List<AddMedicalEmployee> addMedicalEmployees,
                                   List<AddressRegistry> addresses) throws ContingentException;
    
    Long initiateAddMoAddress(long moId, long areaTypeCode, long orderId, List<AddressRegistry> addresses) throws ContingentException;

    Long initiateAddAreaAddress(Long areaId, List<AddressRegistry> addressesRegistry) throws ContingentException;

}
