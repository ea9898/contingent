package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.domain.area.model.area.AddMedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.security.RequestContext;
import org.springframework.scheduling.annotation.Async;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

public interface AreaServiceInternalAsync {

    // Асинхронная инициация процесса распределения жилых домов к территории обслуживания МО (К_УУ_27)
    @Async
    @Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
    void asyncInitiateAddMoAddress(long sysopId, RequestContext requestContext, long moId, long areaTypeCode, long orderId, List<AddressRegistry> addresses) throws ContingentException;

    // Асинхронное создание участка первичного класса (А_УУ_10)
    @Async
    @Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
    void asyncCreatePrimaryArea(RequestContext requestContext, long sysopId, long moId, Long muId, Integer number, String description, Long areaTypeCode,
                                Long areaTypeProfileCode, List<Long> policyTypes, Integer ageMin, Integer ageMax, Integer ageMinM,
                                Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                boolean autoAssignForAttachment, Boolean attachByMedicalReason,
                                List<AddMedicalEmployee> addMedicalEmployees,
                                List<AddressRegistry> addresses);

    @Async
    @Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
    void asyncAddAreaAddress(RequestContext requestContext, long sysopId, Long areaId, List<AddressRegistry> addressesRegistry);
}
