package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.model.area.AddMedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.AddressArea;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import moscow.ptnl.contingent.domain.area.model.area.ChangeMedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.MedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.SearchAreaAddress;
import moscow.ptnl.contingent.error.ContingentException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.util.List;
import java.util.Map;

/**
 * Интерфейс доменного сервиса для методов работы с участками.
 */
public interface AreaService {

    /**
     * (К_УУ_7) Создание участка обслуживания первичного класса
     * Создание участка первичного класса
     * @param moId
     * @param muId
     * @param number
     * @param areaTypeCode
     * @param policyTypesIds
     * @param ageMin
     * @param ageMax
     * @param ageMinM
     * @param ageMaxM
     * @param ageMinW
     * @param ageMaxW
     * @param autoAssignForAttachment
     * @param attachByMedicalReason
     * @param description
     * @return
     * @throws ContingentException
     */
    Long createPrimaryArea(long moId, Long muId, Integer number, Long areaTypeCode, List<Long> policyTypesIds,
                           Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                           boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException;

    Area createPrimaryAreaInternal(long moId, Long muId, Integer number, Long areaTypeCode, List<Long> policyTypesIds,
                                   Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                   boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException;

    /**
     * (К_УУ_8) Создание участка обслуживания зависимого класса
     * Создание участка зависимого класса
     * @param moId
     * @param muId
     * @param number
     * @param areaTypeCode
     * @param primaryAreaTypeCodesIds
     * @param policyTypeCodesIds
     * @param ageMin
     * @param ageMax
     * @param ageMinM
     * @param ageMaxM
     * @param ageMinW
     * @param ageMaxW
     * @param description
     * @return
     * @throws ContingentException
     */
    Long createDependentArea(long moId, Long muId, Integer number, Long areaTypeCode,
                             List<Long> primaryAreaTypeCodesIds, List<Long> policyTypeCodesIds,
                             Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                             String description) throws ContingentException;

    /**
     *(К_УУ_9) Изменение участка обслуживания первичного класса
     * Изменение участка обслуживания первичного класса
     * @param areaId
     * @param number
     * @param policyTypesAddIds
     * @param policyTypesDelIds
     * @param ageMin
     * @param ageMax
     * @param ageMinM
     * @param ageMaxM
     * @param ageMinW
     * @param ageMaxW
     * @param autoAssignForAttachment
     * @param attachByMedicalReason
     * @param description
     * @throws ContingentException
     */
    void updatePrimaryArea(long areaId, Integer number, List<Long> policyTypesAddIds, List<Long> policyTypesDelIds,
                                  Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                  Boolean autoAssignForAttachment, Boolean attachByMedicalReason, String description) throws ContingentException;

    /**
     * (К_УУ_10) Изменение участка обслуживания зависимого класса
     * Изменение участка обслуживания зависимого класса
     * @param areaId
     * @param muId
     * @param number
     * @param description
     * @param primaryAreaTypeCodesAddIds
     * @param primaryAreaTypeCodesDelIds
     * @param policyTypesAddIds
     * @param policyTypesDelIds
     * @param ageMin
     * @param ageMax
     * @param ageMinM
     * @param ageMaxM
     * @param ageMinW
     * @param ageMaxW
     * @throws ContingentException
     */
    void updateDependentArea(long areaId, Long muId, Integer number, String description,
                             List<Long> primaryAreaTypeCodesAddIds, List<Long> primaryAreaTypeCodesDelIds,
                             List<Long> policyTypesAddIds, List<Long> policyTypesDelIds,
                             Integer ageMin, Integer ageMax, Integer ageMinM, Integer ageMaxM, Integer ageMinW, Integer ageMaxW) throws ContingentException;

    /**
     * (К_УУ_11) Изменение медицинских работников на участке обслуживания
     * Изменение медицинских работников на участке обслуживания
     * @param areaId
     * @param addEmployeesInput
     * @param changeEmployeesInput
     * @return
     * @throws ContingentException
     */
    List<Long> setMedicalEmployeeOnArea(long areaId, List<AddMedicalEmployee> addEmployeesInput,
                                        List<ChangeMedicalEmployee> changeEmployeesInput) throws ContingentException;

    List<Long> setMedicalEmployeeOnAreaInternal(long areaId, List<AddMedicalEmployee> addEmployeesInput,
                                                List<ChangeMedicalEmployee> changeEmployeesInput) throws ContingentException;

    /**
     * (К_УУ_12) Получение подробной информации об участке
     * Получение подробной информации об участке по ИД участка
     * @param areaId
     * @return
     * @throws ContingentException
     */
    AreaInfo getAreaById(Long areaId) throws ContingentException;

    /**
     * (К_УУ_13) Добавление адресов на участок обслуживания
     * Добавление адресов на участок обслуживания
     * @param areaId
     * @param addressesRegistry
     * @param limitAddress
     * @return
     * @throws ContingentException
     */
    List<Long> addAreaAddress(Long areaId, List<AddressRegistry> addressesRegistry, boolean limitAddress) throws ContingentException;

    List<Long> addAreaAddressInternal(Long areaId, List<AddressRegistry> addressesRegistry, boolean limitAddress) throws ContingentException;

    /**
     * (К_УУ_14) Удаление адресов из участка обслуживания
     * Удаление адресов из участка обслуживания
     * @param areaId
     * @param areaAddressIds
     * @throws ContingentException
     */
    void delAreaAddress(long areaId, List<Long> areaAddressIds) throws ContingentException;

    /**
     * (К_УУ_15) Получение списка адресов участка обслуживания
     * Метод позволяет получить список адресов участков обслуживания в пределах одного МО
     * @param moId
     * @param areaIds
     * @param paging
     * @return
     * @throws ContingentException
     */
    Page<AreaAddress> getAreaAddress(Long moId, List<Long> areaIds, PageRequest paging) throws ContingentException;

    /**
     * (К_УУ_16) Архивирование участка обслуживания
     * Архивирование участка обслуживания
     * @param areaId
     * @throws ContingentException
     */
    void archiveArea(long areaId) throws ContingentException;

    /**
     * (К_УУ_17) Восстановление архивного участка обслуживания
     * Восстановление участка обслуживания из архива
     * @param areaId
     * @throws ContingentException
     */
    void restoreArea(Long areaId) throws ContingentException;

    /**
     * (К_УУ_21) Распределение жилых домов к территории обслуживания МО
     * Распределение адресов к территории обслуживания медицинской организации
     * @param moId
     * @param areaTypeCode
     * @param orderId
     * @param addressesRegistry
     * @param limitAddress
     * @return
     * @throws ContingentException
     */
    List<Long> addMoAddress(long moId, long areaTypeCode, long orderId, List<AddressRegistry> addressesRegistry, boolean limitAddress)
            throws ContingentException;

    /**
     * (К_УУ_24) Получение идентификатора для создания нового участка
     * Предоставление идентификатора для создания нового участка на стороне Контингент 1
     * @return
     * @throws ContingentException
     */
    Long getNewAreaId() throws ContingentException;

    /**
     * (К_УУ_25) Предоставление списка участков
     * Просмотр списка участков обслуживания
     * @param areaTypeClassCode
     * @param moId
     * @param muIds
     * @param areaTypeCodes
     * @param number
     * @param description
     * @param isArchived
     * @param medicalEmployees
     * @param searchAreaAddresses
     * @param isExactAddressMatch
     * @param paging
     * @return
     * @throws ContingentException
     */
    Page<AreaInfo> searchArea(Long areaTypeClassCode, Long moId, List<Long> muIds, List<Long> areaTypeCodes,
                              Integer number, String description, Boolean isArchived,
                              List<MedicalEmployee> medicalEmployees,
                              List<SearchAreaAddress> searchAreaAddresses, Boolean isExactAddressMatch,
                              PageRequest paging) throws ContingentException;

    /**
     * (К_УУ_26) Инициация процесса создания участка обслуживания первичного класса
     * Метод регистрирует и инициирует асинхронную операцию создания участка первичного класса
     * @param moId
     * @param muId
     * @param number
     * @param description
     * @param areaTypeCode
     * @param policyTypes
     * @param ageMin
     * @param ageMax
     * @param ageMinM
     * @param ageMaxM
     * @param ageMinW
     * @param ageMaxW
     * @param autoAssignForAttachment
     * @param attachByMedicalReason
     * @param addMedicalEmployees
     * @param addresses
     * @return
     * @throws ContingentException
     */
    Long initiateCreatePrimaryArea(long moId, Long muId, Integer number, String description, Long areaTypeCode,
                                   List<Long> policyTypes, Integer ageMin, Integer ageMax, Integer ageMinM,
                                   Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                   boolean autoAssignForAttachment, Boolean attachByMedicalReason,
                                   List<AddMedicalEmployee> addMedicalEmployees,
                                   List<AddressRegistry> addresses) throws ContingentException;

    /**
     * (К_УУ_27) Инициация процесса распределения жилых домов к территории обслуживания МО
     * Метод регистрирует и инициирует асинхронную операцию распределения жилых домов к территории обслуживания МО
     * @param moId
     * @param areaTypeCode
     * @param orderId
     * @param addresses
     * @return
     * @throws ContingentException
     */
    Long initiateAddMoAddress(long moId, long areaTypeCode, long orderId, List<AddressRegistry> addresses) throws ContingentException;

    /**
     * (К_УУ_28) Инициация процесса добавления адресов на участок обслуживания
     * Метод регистрирует и инициирует асинхронную операцию добавления адресов на участок обслуживания
     * @param areaId
     * @param addressesRegistry
     * @return
     * @throws ContingentException
     */
    Long initiateAddAreaAddress(Long areaId, List<AddressRegistry> addressesRegistry) throws ContingentException;

    /**
     * (К_УУ_29) Получение списка участков для ДН
     * @param moId
     * @param muIds
     * @param areaTypeCodes
     * @param specializationCodes
     * @param areaIds
     * @param paging
     * @return
     * @throws ContingentException
     */
    Page<Area> searchDnArea(Long moId, List<Long> muIds, List<Long> areaTypeCodes, List<Long> specializationCodes,
                            List<Long> areaIds, PageRequest paging) throws ContingentException;

    /**
     * (К_УУ_31) Краткое предоставление списка участков
     * @param areaIds
     * @param paging
     * @return
     * @throws ContingentException
     */
    Page<Area> getAreaListBrief(List<Long> areaIds, PageRequest paging) throws ContingentException;

    /**
     * (К_УУ_30) Поиск МУ по адресу обслуживания (4. Передан блок searchByCode)
     * @param areaTypeCodes
     * @param areaOMKTECode
     * @param regionOMKTECode
     * @param paging
     * @return
     * @throws ContingentException
     */
    Page<Area> searchMuByAreaAddress(List<Long> areaTypeCodes, String areaOMKTECode, String regionOMKTECode,
                                     PageRequest paging) throws ContingentException;

    /**
     * (К_УУ_30) Поиск МУ по адресу обслуживания (3. Передан блок searchByNsiGlobalId)
     * @param areaTypeCodes
     * @param aoLevel
     * @param globalIdNsi
     * @param paging
     * @return
     * @throws ContingentException
     */
    Page<Area> searchMuByAreaAddress(List<Long> areaTypeCodes, String aoLevel, long globalIdNsi,
                                     PageRequest paging) throws ContingentException;

    /**
     * (К_УУ_НСИ_3) Редактирование параметров адреса
     * @param arGlobalId
     * @param fields
     * @return
     * @throws ContingentException
     */
    void editAddress(Long arGlobalId, Map<String, String> fields) throws ContingentException;
}
