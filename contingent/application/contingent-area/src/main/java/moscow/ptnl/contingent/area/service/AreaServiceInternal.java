package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.transform.SearchAreaAddress;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import ru.mos.emias.contingent2.address.AddressRegistryBaseType;
import ru.mos.emias.contingent2.area.types.SearchAreaRequest;
import ru.mos.emias.contingent2.core.AddMedicalEmployee;
import ru.mos.emias.contingent2.core.ChangeMedicalEmployee;


import java.util.List;

public interface AreaServiceInternal {

    Long createPrimaryArea(long moId, Long muId, Integer number, Long areaTypeCode, List<Long> policyTypes,
                           Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                           boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException;

    Long createDependentArea(long moId, Long muId, Integer number, Long areaTypeCode,
                             List<Long> primaryAreaTypeCodes, List<Long> policyTypeCodes,
                             Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                             String description) throws ContingentException;

    void updatePrimaryArea(long areaId, Integer number, List<Long> policyTypesAdd, List<Long> policyTypesDel,
                           Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                           Boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException;

    void updateDependentArea(long areaId, Long muId, Integer number, String description,
                             List<Long> primaryAreaTypeCodesAdd, List<Long> primaryAreaTypeCodesDel,
                             List<Long> policyTypesAdd, List<Long> policyTypesDel,
                             Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW) throws ContingentException;

    List<Long> setMedicalEmployeeOnArea(long areaId, List<AddMedicalEmployee> addMedicalEmployees,
                                        List<ChangeMedicalEmployee> changeMedicalEmployees) throws ContingentException;

    void restoreArea(Long areaId) throws ContingentException;

    List<Long> addAreaAddress(Long areaId, List<AddressRegistryBaseType> addressesRegistry, boolean limitAddress) throws ContingentException;

    List<Long> addMoAddress(long moId, long areaTypeCode, long orderId, List<AddressRegistryBaseType> addresses, boolean limitAddress) throws ContingentException;

    void archiveArea(long areaId) throws ContingentException;

    void delAreaAddress(long areaId, List<Long> areaAddressIds) throws ContingentException;

    Page<AreaInfo> searchArea(Long areaTypeClassCode, Long moId, List<Long> muIds, List<Long> areaTypeCodes,
                        Integer number, String description, Boolean isArchived, List<SearchAreaRequest.MedicalEmployee> medicalEmployees,
                        List<SearchAreaAddress> addresses, Boolean isExactAddressMatch, PageRequest paging) throws ContingentException;

    Long initiateCreatePrimaryArea(long moId, Long muId, Integer number, String description, Long areaTypeCode,
                                   List<Long> policyTypes, Integer ageMin, Integer ageMax, Integer ageMinM,
                                   Integer ageMaxM, Integer ageMinW, Integer ageMaxW, boolean autoAssignForAttachment,
                                   Boolean attachByMedicalReason, List<AddMedicalEmployee> addMedicalEmployees,
                                   List<AddressRegistryBaseType> addresses) throws ContingentException;
    
    Long initiateAddMoAddress(long moId, long areaTypeCode, long orderId, List<AddressRegistryBaseType> addresses) throws ContingentException;

    Long initiateAddAreaAddress(Long areaId, List<AddressRegistryBaseType> addressesRegistry) throws ContingentException;

}
