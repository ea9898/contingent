package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrders;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.model.area.AreaInfo;
import moscow.ptnl.contingent.area.model.area.AreaTypeStateType;
import moscow.ptnl.contingent.area.model.area.MuAreaTypesFull;
import moscow.ptnl.contingent.area.model.area.NotNsiAddress;
import moscow.ptnl.contingent.area.model.area.NsiAddress;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import ru.mos.emias.contingent2.core.AddMedicalEmployee;
import ru.mos.emias.contingent2.core.ChangeMedicalEmployee;
import ru.mos.emias.contingent2.core.MuType;


import java.time.LocalDate;
import java.util.List;

public interface AreaServiceInternal {

    Long createPrimaryArea(long moId, long muId, Integer number, Long areaTypeCode, List<Long> policyTypes,
                           Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                           boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException;

    Long createDependentArea(long moId, Long muId, Integer number, Long areaTypeCode,
                             List<Long> primaryAreaTypeCodes, List<Long> policyTypeCodes,
                             Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                             boolean autoAssignForAttachment, String description) throws ContingentException;

    void updatePrimaryArea(long areaId, Integer number, List<Long> policyTypesAdd, List<Long> policyTypesDel,
                           Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                           boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException;

    void updateDependentArea(long areaId, Long muId, Integer number, String description,
                             List<Long> primaryAreaTypeCodesAdd, List<Long> primaryAreaTypeCodesDel,
                             List<Long> policyTypesAdd, List<Long> policyTypesDel,
                             Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                             Boolean autoAssignForAttachment) throws ContingentException;

    Long createOrder(String number, LocalDate date, String ouz, String name) throws ContingentException;

    void updateOrder(Long id, String number, LocalDate date, String ouz, String name) throws ContingentException;

    Page<AddressAllocationOrders> searchOrder(Long id, String number, LocalDate date, String name, PageRequest paging) throws ContingentException;

    AreaInfo getAreaById(Long areaId) throws ContingentException;

    List<Long> setMedicalEmployeeOnArea(long areaId, List<AddMedicalEmployee> addMedicalEmployees,
                                        List<ChangeMedicalEmployee> changeMedicalEmployees) throws ContingentException;

    void restoreArea(Long areaId) throws ContingentException;

    Long getNewAreaId() throws ContingentException;

    List<Long> addAreaAddress(Long areaId, List<NsiAddress> nsiAddresses,
                              List<NotNsiAddress> notNsiAddresses) throws ContingentException;

    List<Long> addMoAddress(long moId, long areaTypeCode, long orderId, List<NsiAddress> nsiAddresses,
                            List<NotNsiAddress> notNsiAddresses) throws ContingentException;

    Page<MoAddress> getMoAddress(long moId, List<Long> areaTypeCodes, PageRequest paging) throws ContingentException;

    Page<moscow.ptnl.contingent.area.model.area.AddressArea> getAreaAddress(long areaId, PageRequest paging) throws ContingentException;

    void delMoAddress(List<Long> moAddressIds, long orderId) throws ContingentException;

    void archiveArea(long areaId) throws ContingentException;

    void delAreaAddress(long areaId, List<Long> areaAddressIds) throws ContingentException;

    void addMoAvailableAreaTypes(long moId, List<Long> areaTypeCodes) throws ContingentException;

    void delMoAvailableAreaTypes(long moId, List<Long> areaTypeCodes) throws ContingentException;

    List<AreaType> getMoAvailableAreaTypes(long moId) throws ContingentException;

    void addMuAvailableAreaTypes(long moId, long muId, List<Long> areaTypeCodes) throws ContingentException;

    void delMuAvailableAreaTypes(long muId, List<Long> areaTypeCodes) throws ContingentException;

    MuAreaTypesFull getMuAvailableAreaTypes(long moId, long muId, AreaTypeStateType areaTypeState) throws ContingentException;
}
