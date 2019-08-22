package area.service;


import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.area.MuAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.AreaPolicyTypes;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClass;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeKind;
import moscow.ptnl.contingent.area.entity.nsi.PolicyType;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.service.AreaServiceHelper;

import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.area.MuAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.nsi.AreaPolicyTypesRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypesCRUDRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.doReturn;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes= {MockConfiguration.class, MockRepositoriesConfiguration.class})
public class AreaServiceHelperTest {

    @Autowired
    private AreaServiceHelper areaServiceHelper;

    @Autowired
    public AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    public AreaRepository areaRepository;

    @Autowired
    public AreaCRUDRepository areaCRUDRepository;

    @Autowired
    public MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    @Autowired
    public MuAvailableAreaTypesRepository muAvailableAreaTypesRepository;

    @Autowired
    public AreaPolicyTypesRepository areaPolicyTypesRepository;

    //Тестовые данные
    private AreaType areaTypePrimary1;
    private AreaType areaTypeDependent1;
    private AreaToAreaType areaToAreaType1;
    private MoAvailableAreaTypes moAvailableAreaTypes;
    private MuAvailableAreaTypes muAvailableAreaTypes;
    private Area areaPrimary1;
    private Area areaDependent1;
    private PolicyType policyType1;
    private PolicyType policyType2;
    private AreaPolicyTypes areaPolicyType;
    private Long moId = 204L;
    private Long muId = 100L;

    @BeforeEach
    public void setup() {
        AreaTypeKind areaTypeKind1 = new AreaTypeKind();
        areaTypeKind1.setCode(1L);
        areaTypeKind1.setTitle("test");
        areaTypeKind1.setArchived(false);
        AreaTypeClass areaTypeClass1 = new AreaTypeClass();
        areaTypeClass1.setCode(1L);
        areaTypeClass1.setTitle("Primary test");
        areaTypeClass1.setArchived(false);
        AreaTypeClass areaTypeClass2 = new AreaTypeClass();
        areaTypeClass2.setCode(10L);
        areaTypeClass2.setTitle("Dependent test");
        areaTypeClass2.setArchived(false);
        areaTypePrimary1 = new AreaType(3L, "Терапевтический", false);
        areaTypePrimary1.setAreaTypeClass(areaTypeClass1);
        areaTypePrimary1.setAreaTypeKind(areaTypeKind1);
        areaTypePrimary1.setAreaCountLimit(2);
        areaTypePrimary1.setMpguAvailable(false);
        areaTypePrimary1.setAttachByMedicalReason(true);
        areaTypeDependent1 = new AreaType(5L, "Терапевтический 2", false);
        areaTypeDependent1.setAreaTypeClass(areaTypeClass2);
        areaTypeDependent1.setAreaCountLimit(3);
        areaPrimary1 = new Area(3L, moId, null, areaTypePrimary1, false, LocalDateTime.now());
        areaPrimary1.setNumber(123);
        areaDependent1 = new Area(5L, moId, null, areaTypeDependent1, false, LocalDateTime.now());
        areaDependent1.setNumber(124);
        areaDependent1.setAgeMin(1);
        areaDependent1.setAgeMax(16);
        areaToAreaType1 = new AreaToAreaType();
        areaToAreaType1.setId(1L);
        areaToAreaType1.setArea(areaDependent1);
        areaToAreaType1.setAreaType(areaTypeDependent1);
        moAvailableAreaTypes = new MoAvailableAreaTypes();
        moAvailableAreaTypes.setAreaType(areaTypePrimary1);
        moAvailableAreaTypes.setCreateDate(LocalDateTime.now());
        moAvailableAreaTypes.setId(1L);
        moAvailableAreaTypes.setMoId(moId);
        muAvailableAreaTypes = new MuAvailableAreaTypes();
        muAvailableAreaTypes.setAreaType(areaTypeDependent1);
        muAvailableAreaTypes.setCreateDate(LocalDateTime.now());
        muAvailableAreaTypes.setId(1L);
        muAvailableAreaTypes.setMuId(muId);
        muAvailableAreaTypes.setMoAvailableAreaType(moAvailableAreaTypes);
        moAvailableAreaTypes.setMuAvailableAreaTypes(Collections.singleton(muAvailableAreaTypes));
        policyType1 = new PolicyType();
        policyType1.setCode(1L);
        policyType1.setTitle("Test policy type");
        policyType1.setArchived(false);
        policyType2 = new PolicyType();
        policyType2.setCode(3L);
        policyType2.setTitle("Test policy type 2");
        policyType2.setArchived(false);
        areaPolicyType = new AreaPolicyTypes();
        areaPolicyType.setId(1L);
        areaPolicyType.setArea(areaPrimary1);
        areaPolicyType.setPolicyType(policyType1);
    }

    @Test
    public void checkAgeSetupRange() {
        Validation validation = new Validation();
        areaServiceHelper.checkAgeSetupRange(0, 5, 1, 2, "ageMin", "ageMax", validation);
        assertFalse(validation.isSuccess());
    }

    @Test
    void checkAndGetAreaTypesExist() {
        doReturn(Optional.of(areaTypePrimary1)).when(areaTypesCRUDRepository).findById(areaTypePrimary1.getCode());
        doReturn(Optional.of(areaTypeDependent1)).when(areaTypesCRUDRepository).findById(areaTypeDependent1.getCode());
        Validation validation = new Validation();
        List<AreaType> areaTypeList = areaServiceHelper.checkAndGetAreaTypesExist(Arrays.asList(3L, 5L), validation);
        assertTrue(validation.isSuccess());
        assertEquals(areaTypeList.size(), 2);
        validation = new Validation();
        areaTypeList = areaServiceHelper.checkAndGetAreaTypesExist(Collections.singletonList(1234L), validation);
        assertFalse(validation.isSuccess());
        assertEquals(areaTypeList.size(), 0);
    }

    @Test
    void checkAreaTypeIsPrimary() {
        Validation validation = new Validation();
        areaServiceHelper.checkAreaTypeIsPrimary(areaTypePrimary1, validation);
        assertTrue(validation.isSuccess());
        validation = new Validation();
        areaServiceHelper.checkAreaTypeIsPrimary(areaTypeDependent1, validation);
        assertFalse(validation.isSuccess());
    }

    @Test
    void checkAndGetArea() {
        doReturn(Optional.of(areaPrimary1)).when(areaCRUDRepository).findById(areaPrimary1.getId());
        Validation validation = new Validation();
        areaServiceHelper.checkAndGetArea(areaPrimary1.getId(), validation);
        assertTrue(validation.isSuccess());
        validation = new Validation();
        areaPrimary1.setArchived(true);
        areaServiceHelper.checkAndGetArea(areaPrimary1.getId(), validation);
        assertFalse(validation.isSuccess());
        validation = new Validation();
        areaServiceHelper.checkAndGetArea(11111, validation);
        assertFalse(validation.isSuccess());
    }

    @Test
    void checkEmptyMuId() {
        Throwable exception = assertThrows(ContingentException.class, () -> areaServiceHelper.checkEmptyMuId(null, areaTypePrimary1));
        assertEquals(exception.getMessage(), "Не указан филиал МО");
        assertDoesNotThrow(() -> areaServiceHelper.checkEmptyMuId(muId, areaTypePrimary1));
    }

    @Test
    void checkAreaTypeAvailable() {
        doReturn(Collections.singletonList(muAvailableAreaTypes)).when(muAvailableAreaTypesRepository).findByAreaTypes(areaTypeDependent1, muId);
        doReturn(Collections.singletonList(moAvailableAreaTypes)).when(moAvailableAreaTypesRepository).findByAreaTypes(areaTypePrimary1, moId);
        Validation validation = new Validation();
        Throwable exception = assertThrows(ContingentException.class, () -> areaServiceHelper.checkAreaTypeAvailable(111, null, areaTypePrimary1, validation));
        assertEquals(exception.getMessage(), "Невозможно создать/изменить участок, так как тип участка " + areaTypePrimary1.getTitle() + " отсутствует в списке разрешённых");
        validation.reset();
        assertDoesNotThrow(() -> areaServiceHelper.checkAreaTypeAvailable(moId, null, areaTypePrimary1, validation));
        validation.reset();
        exception = assertThrows(ContingentException.class, () -> areaServiceHelper.checkAreaTypeAvailable(111, 111L, areaTypeDependent1, validation));
        assertEquals(exception.getMessage(), "Невозможно создать/изменить участок, так как тип участка " + areaTypeDependent1.getTitle() + " отсутствует в списке разрешённых");
        validation.reset();
        assertDoesNotThrow(() -> areaServiceHelper.checkAreaTypeAvailable(moId, muId, areaTypeDependent1, validation));
    }

    @Test
    void checkAreaTypeCountLimits() {
        doReturn(Collections.singletonList(areaPrimary1)).when(areaRepository).findAreas(moId, null, areaTypePrimary1.getCode(), null, true);
        doReturn(Collections.singletonList(areaDependent1)).when(areaRepository).findAreas(null, muId, areaTypeDependent1.getCode(), null, true);
        Validation validation = new Validation();
        Throwable exception = assertThrows(ContingentException.class, () -> areaServiceHelper.checkAreaTypeCountLimits(moId, null, areaTypePrimary1, validation));
        assertEquals(exception.getMessage(), "Невозможно создать участок, так как количество участков с типом " + areaTypePrimary1.getTitle() + " превысит ограничение (Не более одного на МО)");
        validation.reset();
        assertDoesNotThrow(() -> areaServiceHelper.checkAreaTypeCountLimits(111, null, areaTypePrimary1, validation));
        validation.reset();
        exception = assertThrows(ContingentException.class, () -> areaServiceHelper.checkAreaTypeCountLimits(moId, muId, areaTypeDependent1, validation));
        assertEquals(exception.getMessage(), "Невозможно создать участок, так как количество участков с типом " + areaTypeDependent1.getTitle() + " превысит ограничение (Не более одного на МУ)");
        validation.reset();
        assertDoesNotThrow(() -> areaServiceHelper.checkAreaTypeCountLimits(111, 111L, areaTypeDependent1, validation));
    }

    @Test
    void checkAreaExistsInMU() {
        doReturn(Collections.singletonList(areaPrimary1)).when(areaRepository).findAreas(moId, null, areaTypePrimary1.getCode(), 123, true);
        doReturn(Collections.singletonList(areaDependent1)).when(areaRepository).findAreas(null, muId, areaTypeDependent1.getCode(), 124, true);
        Validation validation = new Validation();
        Throwable exception = assertThrows(ContingentException.class, () -> {
            areaServiceHelper.checkAreaExistsInMU(muId, moId, areaTypeDependent1, 124, null, validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), "Участок обслуживания с типом " + areaTypeDependent1.getTitle() + " и номером 124 уже существует в рамках филиала МО");
        validation.reset();
        assertDoesNotThrow(() -> areaServiceHelper.checkAreaExistsInMU(muId, moId, areaTypePrimary1, 125, null, validation));
        validation.reset();
        exception = assertThrows(ContingentException.class, () -> {
            areaServiceHelper.checkAreaExistsInMU(null, moId, areaTypePrimary1, 123, null, validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), "Участок обслуживания с типом " + areaTypePrimary1.getTitle() + " и номером 123 уже существует в рамках филиала МО");
    }

    @Test
    void checkPolicyTypesIsOMS() {
        Validation validation = new Validation();
        Throwable exception = assertThrows(ContingentException.class, () -> {
            areaServiceHelper.checkPolicyTypesIsOMS(Arrays.asList(4L, 8L), validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), "Невозможно создать/изменить участок, так как добавляемый тип полиса отличен от ОМС");
        validation.reset();
        assertDoesNotThrow(() -> areaServiceHelper.checkPolicyTypesIsOMS(Arrays.asList(1L), validation));
    }

    @Test
    void checkPolicyTypesDel() {
        doReturn(Collections.singletonList(areaPolicyType)).when(areaPolicyTypesRepository).findAll(areaPrimary1, policyType1);
        Validation validation = new Validation();
        Throwable exception = assertThrows(ContingentException.class, () -> {
            areaServiceHelper.checkPolicyTypesDel(areaPrimary1, Arrays.asList(policyType2), validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), "Тип полиса с кодом " + policyType2.getCode() + " не задан для участка с ИД " + areaPrimary1.getId());
        validation.reset();
        assertDoesNotThrow(() -> areaServiceHelper.checkPolicyTypesDel(areaPrimary1, Arrays.asList(policyType1), validation));
    }

    @Test
    void checkAutoAssignForAttachment() {
        Validation validation = new Validation();
        Throwable exception = assertThrows(ContingentException.class, () -> {
            areaServiceHelper.checkAutoAssignForAttachment(areaTypeDependent1, true, true, validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), "Для участка не может быть одновременно установлены признаки «Назначать для автоматического прикрепления» и «Необходимость медицинских показаний»");
        validation.reset();
        exception = assertThrows(ContingentException.class, () -> {
            areaServiceHelper.checkAutoAssignForAttachment(areaTypePrimary1, true, false, validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), "Невозможно установить признак для автоматического прикрепления, т.к. для данного типа участка (Терапевтический) не разрешено прикрепление через МПГУ");
        validation.reset();
        assertDoesNotThrow(() -> areaServiceHelper.checkAutoAssignForAttachment(areaTypePrimary1, false, true, validation));
        assertDoesNotThrow(() -> areaServiceHelper.checkAutoAssignForAttachment(areaTypeDependent1, true, false, validation));
    }

    @Test
    void checkAttachByMedicalReason() {
        Validation validation = new Validation();
        Throwable exception = assertThrows(ContingentException.class, () -> {
            areaServiceHelper.checkAttachByMedicalReason(areaTypePrimary1, false, validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), "Значение признака «Необходимость медицинских показаний» (false) не соответствует настройкам типа участка (true)");
        validation.reset();
        areaTypePrimary1.setAttachByMedicalReason(false);
        exception = assertThrows(ContingentException.class, () -> {
            areaServiceHelper.checkAttachByMedicalReason(areaTypePrimary1, true, validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), "Значение признака «Необходимость медицинских показаний» (true) не соответствует настройкам типа участка (false)");
        validation.reset();
        assertDoesNotThrow(() -> areaServiceHelper.checkAttachByMedicalReason(areaTypePrimary1, true, validation));
        assertDoesNotThrow(() -> areaServiceHelper.checkAttachByMedicalReason(areaTypeDependent1, null, validation));
    }

    private void throwValidation(Validation validation) throws ContingentException {
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

    }
}
