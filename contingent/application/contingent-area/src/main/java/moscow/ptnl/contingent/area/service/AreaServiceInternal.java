package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrders;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.error.ContingentException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import ru.mos.emias.contingent2.core.AddMedicalEmployee;
import ru.mos.emias.contingent2.core.ChangeMedicalEmployee;
import ru.mos.emias.contingent2.core.MuType;
import ru.mos.emias.contingent2.core.NotNsiAddress;
import ru.mos.emias.contingent2.core.NsiAddress;

import java.time.LocalDate;
import java.util.List;

public interface AreaServiceInternal {

    List<AreaType> getProfileMU(long muId, long muTypeId) throws ContingentException;

    void addProfileMU(Long muId, Long muTypeId, List<Long> areaTypeCodes) throws ContingentException;

    void delProfileMU(Long muId, List<Long> areaTypeCodes) throws ContingentException;

    Long createPrimaryArea(long moId, long muId, Integer number, Long areaTypeCode,
                           Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                           boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException;

    Long createDependentArea(long moId, Long muId, List<MuType> muTypes, Integer number, Long areaTypeCode, List<Long> primaryAreaTypeCodes,
                             Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                             boolean autoAssignForAttachment, String description) throws ContingentException;

    void updatePrimaryArea(long areaId, Integer number,
                           Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                           boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException;

    void updateDependentArea(long areaId, Long muId, List<MuType> muTypes, Integer muNumber, List<Long> primaryAreaTypeCodesAdd,
                             List<Long> primaryAreaTypeCodesDel,
                             Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                             boolean autoAssignForAttachment, String description) throws ContingentException;

    Long createOrder(String number, LocalDate date, String ouz, String name) throws ContingentException;

    void updateOrder(Long id, String number, LocalDate date, String ouz, String name) throws ContingentException;

    Page<AddressAllocationOrders> searchOrder(Long id, String number, LocalDate date, String name, PageRequest paging) throws ContingentException;

    Area getAreaById(Long id) throws ContingentException;

    List<Long> setMedicalEmployeeOnArea(long areaId, List<AddMedicalEmployee> addMedicalEmployees,
                                        List<ChangeMedicalEmployee> changeMedicalEmployees) throws ContingentException;

    void restoreArea(Long id) throws ContingentException;

    Long getNewAreaId() throws ContingentException;

    List<Long> addAreaAddress();

    List<Long> addMoAddress(long moId, long areaTypeCode, long orderId, List<NsiAddress> nsiAddresses,
                            List<NotNsiAddress> notNsiAddresses) throws ContingentException;

    Page<MoAddress> getMoAddress(long moId, List<Long> areaTypeCodes, PageRequest paging) throws ContingentException;

    void delMoAddress(List<Long> moAddressIds, long orderId) throws ContingentException;
}
