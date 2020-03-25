package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.domain.area.Algorithms;
import moscow.ptnl.contingent.domain.area.EsuHelperService;
import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.domain.area.model.area.AddMedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import moscow.ptnl.contingent.domain.area.model.sysop.SysopMethodType;
import moscow.ptnl.contingent.area.transform.AreaAddressClone;
import moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent;
import moscow.ptnl.contingent.domain.esu.event.annotation.LogESU;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeMedicalPositionsRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeSpecializationsRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.nsi.domain.repository.PolicyTypeRepository;
import moscow.ptnl.contingent.nsi.domain.repository.PositionNomRepository;
import moscow.ptnl.contingent.nsi.repository.SpecializationCRUDRepository;
import moscow.ptnl.contingent.repository.area.AddressAllocationOrderCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaAddressPagingAndSortingRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaAddressRepository;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaMedicalEmployeeCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaMedicalEmployeeRepository;
import moscow.ptnl.contingent.repository.area.AreaPolicyTypesCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaPolicyTypesRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaRepository;
import moscow.ptnl.contingent.repository.area.AreaToAreaTypeCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaToAreaTypeRepository;
import moscow.ptnl.contingent.repository.area.MoAddressCRUDRepository;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.ws.security.UserContextHolder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class AreaServiceInternalImpl implements AreaServiceInternal {

    private final static Logger LOG = LoggerFactory.getLogger(AreaServiceInternalImpl.class);

    @Autowired
    private AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    @Autowired
    private AreaToAreaTypeCRUDRepository areaToAreaTypeCRUDRepository;

    @Autowired
    private AreaToAreaTypeRepository areaToAreaTypeRepository;

    @Autowired
    private AddressAllocationOrderCRUDRepository addressAllocationOrderCRUDRepository;

    @Autowired
    private AreaMedicalEmployeeCRUDRepository areaMedicalEmployeeCRUDRepository;

    @Autowired
    private AreaTypeMedicalPositionsRepository areaTypeMedicalPositionsRepository;

    @Autowired
    private AreaMedicalEmployeeRepository areaMedicalEmployeeRepository;

    @Autowired
    private MoAddressCRUDRepository moAddressCRUDRepository;

    @Autowired
    private AreaAddressRepository areaAddressRepository;

    @Autowired
    private AreaAddressPagingAndSortingRepository areaAddressPagingAndSortingRepository;

    @Autowired
    private AreaServiceHelper areaHelper;

    @Autowired
    private EsuHelperService esuHelperService;

    @Autowired
    private SettingService settingService;

    @Autowired
    private Algorithms algorithms;

    @Autowired
    private PositionNomRepository positionNomRepository;

    @Autowired
    private AreaTypeSpecializationsRepository areaTypeSpecializationsRepository;

    @Autowired
    private AreaPolicyTypesCRUDRepository areaPolicyTypesCRUDRepository;

    @Autowired
    private AreaPolicyTypesRepository areaPolicyTypesRepository;

    @Autowired
    private PolicyTypeRepository policyTypeRepository;

    @Autowired
    private AreaAddressClone areaAddressClone;

    @Autowired
    private HistoryServiceHelperImpl historyHelper;

    @Autowired
    private AreaServiceInternalImplAsync asyncService;

    @Autowired
    private SpecializationCRUDRepository specializationCRUDRepository;

    public AreaServiceInternalImpl() {
    }

    public AreaServiceInternalImpl(Algorithms algorithms, AreaServiceHelper areaHelper) {
        this.algorithms = algorithms;
        this.areaHelper = areaHelper;
    }

    // (К_УУ_26) Инициация процесса создания участка обслуживания первичного класса
    @Override
    public Long initiateCreatePrimaryArea(long moId, Long muId, Integer number, String description, Long areaTypeCode,
                                          List<Long> policyTypes, Integer ageMin, Integer ageMax, Integer ageMinM,
                                          Integer ageMaxM, Integer ageMinW, Integer ageMaxW,
                                          boolean autoAssignForAttachment, Boolean attachByMedicalReason,
                                          List<AddMedicalEmployee> addMedicalEmployees,
                                          List<AddressRegistry> addresses) throws ContingentException {

        // 2
        long sysopId = algorithms.sysOperationRegistration(SysopMethodType.INITIATE_CREATE_PRIMARY_AREA);
        
        // 3
        asyncService.asyncCreatePrimaryArea(UserContextHolder.getContext(), sysopId, moId, muId, number, description,
                        areaTypeCode, policyTypes, ageMin, ageMax, ageMinM, ageMaxM, ageMinW, ageMaxW,
                        autoAssignForAttachment, attachByMedicalReason, addMedicalEmployees, addresses);

        // 4 выполняется автоматически после выполнения createPrimaryArea, setMedicalEmployeeOnArea, addAreaAddress

        // 5
        return sysopId;
    }

    // (К_УУ_27) Инициация процесса распределения жилых домов к территории обслуживания МО
    @Override
    public Long initiateAddMoAddress(long moId, long areaTypeCode, long orderId, List<AddressRegistry> addresses) throws ContingentException {
        //1. Система выполняет проверку полномочий пользователя.
        // Реализовано через аннотацию

        //2. Система выполняет регистрацию новой асинхронной операции
        long sysopId = algorithms.sysOperationRegistration(SysopMethodType.INITIATE_ADD_MO_ADDRESS);

        //3. Система инициирует процесс (выполняется асинхронно) распределения жилых домов к территории обслуживания МО. (А_УУ_11)
        asyncService.asyncInitiateAddMoAddress(sysopId, UserContextHolder.getContext(), moId, areaTypeCode, orderId, addresses);

        //4. Система инициирует процесс журналирования (выполняется асинхронно) по инициации распределения жилых домов к территории обслуживания МО. (А_УУ_8), п.1
        // происходит в п 3 (не забываем добавлять имя метода в AreaServiceLogMethodsEnum)

        //5. Система возвращает в качестве результата: ИД операции
         return sysopId;
    }

    // (К_УУ_28) Инициация процесса добавления адресов на участок обслуживания
    @Override
    public Long initiateAddAreaAddress(Long areaId, List<AddressRegistry> addressesRegistry) throws ContingentException {
        //1. Система выполняет проверку полномочий пользователя.
        // Реализовано через аннотацию
        
        // 2. Система выполняет регистрацию новой асинхронной операции
        long sysopId = algorithms.sysOperationRegistration(SysopMethodType.INITIATE_ADD_AREA_ADDRESS);

        // 3. Система инициирует процесс (выполняется асинхронно) добавления адресов на участок обслуживания.
        asyncService.asyncAddAreaAddress(UserContextHolder.getContext(), sysopId, areaId, addressesRegistry);

        // 4. Система инициирует процесс журналирования (выполняется асинхронно) по инициации добавления адресов на участок обслуживания.
        // выполняется в п 3 (не забываем добавлять имя метода в AreaServiceLogMethodsEnum)

        // 5. Система возвращает в качестве результата: ИД операции
        return sysopId;
    }

}
