package area.service;


import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaToAreaType;
import moscow.ptnl.contingent.domain.area.entity.MoAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.entity.MuAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.entity.AreaPolicyTypes;
import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.domain.area.heplers.AreaHelper;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeClass;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKind;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeRelations;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeRelationsRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypesRepository;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaRepository;
import moscow.ptnl.contingent.domain.area.repository.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.domain.area.repository.MuAvailableAreaTypesRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaPolicyTypesRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesCRUDRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
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
    private AreaHelper areaHelper;

    @Autowired
    public AreaTypesRepository areaTypesRepository;

    @Autowired
    public AreaRepository areaRepository;

    @Autowired
    public MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    @Autowired
    public MuAvailableAreaTypesRepository muAvailableAreaTypesRepository;

    @Autowired
    public AreaPolicyTypesRepository areaPolicyTypesRepository;

    @Autowired
    private AreaTypeRelationsRepository areaTypeRelationsRepository;

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
    private AreaTypeRelations areaTypeRelations;
    private Long moId = 204L;
    private Long muId = 100L;
    private Integer areaPrimary1Number = 123;

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
        areaTypeClass2.setCode(2L);
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
        areaPrimary1 = Area.builder()
                .id(3L)
                .moId(moId)
                .areaType(areaTypePrimary1)
                .archived(false)
                .createDate(LocalDateTime.now())
                .number(areaPrimary1Number)
                .build();
        areaDependent1 = Area.builder()
                .id(5L)
                .moId(moId)
                .areaType(areaTypeDependent1)
                .archived(false)
                .createDate(LocalDateTime.now())
                .number(124)
                .ageMin(1)
                .ageMax(16)
                .build();
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
        areaTypeRelations = new AreaTypeRelations();
        areaTypeRelations.setDependentAreaType(areaTypeDependent1);
        areaTypeRelations.setPrimaryAreaType(areaTypePrimary1);
        areaTypeRelations.setArchived(false);
    }

    @Test
    public void checkAgeSetupRange() {
        Validation validation = new Validation();
        areaHelper.checkAgeSetupRange(0, 5, 1, 2, "ageMin", "ageMax", validation);
        assertFalse(validation.isSuccess());
    }

    @Test
    void checkAndGetAreaTypesExist() {
        doReturn(Optional.of(areaTypePrimary1)).when(areaTypesRepository).findById(areaTypePrimary1.getCode());
        doReturn(Optional.of(areaTypeDependent1)).when(areaTypesRepository).findById(areaTypeDependent1.getCode());
        Validation validation = new Validation();
        List<AreaType> areaTypeList = areaHelper.checkAndGetAreaTypesExist(Arrays.asList(3L, 5L), validation);
        assertTrue(validation.isSuccess());
        assertEquals(areaTypeList.size(), 2);
        validation = new Validation();
        areaTypeList = areaHelper.checkAndGetAreaTypesExist(Collections.singletonList(1234L), validation);
        assertFalse(validation.isSuccess());
        assertEquals(areaTypeList.size(), 0);
    }

    @Test
    void checkAreaTypeIsPrimary() {
        Validation validation = new Validation();
        areaHelper.checkAreaTypeIsPrimary(areaTypePrimary1, validation);
        assertTrue(validation.isSuccess());
        validation = new Validation();
        areaHelper.checkAreaTypeIsPrimary(areaTypeDependent1, validation);
        assertFalse(validation.isSuccess());
    }

    @Test
    void checkAreaTypeIsDependent() {
        Validation validation = new Validation();
        areaHelper.checkAreaTypeIsDependent(areaTypeDependent1, validation);
        assertTrue(validation.isSuccess());
        validation = new Validation();
        areaHelper.checkAreaTypeIsDependent(areaTypePrimary1, validation);
        assertFalse(validation.isSuccess());
    }

    @Test
    void checkAreaTypeRelations() {
        doReturn(Optional.of(areaTypeRelations)).when(areaTypeRelationsRepository).getByDependentAndPrimaryAreaTypes(areaTypeDependent1, areaTypePrimary1);
        Validation validation = new Validation();
        areaHelper.checkAreaTypeRelations(areaTypeDependent1, areaTypePrimary1, validation);
        assertTrue(validation.isSuccess());
        doReturn(Optional.empty()).when(areaTypeRelationsRepository).getByDependentAndPrimaryAreaTypes(areaTypeDependent1, areaTypePrimary1);
        validation = new Validation();
        areaHelper.checkAreaTypeRelations(areaTypeDependent1, areaTypePrimary1, validation);
        assertFalse(validation.isSuccess());
    }

    @Test
    void checkAndGetArea() {
        doReturn(Optional.of(areaPrimary1)).when(areaRepository).findById(areaPrimary1.getId());
        Validation validation = new Validation();
        areaHelper.checkAndGetArea(areaPrimary1.getId(), validation);
        assertTrue(validation.isSuccess());
        validation = new Validation();
        areaPrimary1.setArchived(true);
        areaHelper.checkAndGetArea(areaPrimary1.getId(), validation);
        assertFalse(validation.isSuccess());
        validation = new Validation();
        areaHelper.checkAndGetArea(11111, validation);
        assertFalse(validation.isSuccess());
    }

    @Test
    void checkEmptyMuId() {
        Throwable exception = assertThrows(ContingentException.class, () -> areaHelper.checkEmptyMuId(null, areaTypePrimary1));
        assertEquals(exception.getMessage(), "Не указан филиал МО");
        assertDoesNotThrow(() -> areaHelper.checkEmptyMuId(muId, areaTypePrimary1));
    }

    @Test
    void checkAreaTypeAvailable() {
        doReturn(Collections.singletonList(muAvailableAreaTypes)).when(muAvailableAreaTypesRepository).findByAreaTypes(areaTypeDependent1, muId);
        doReturn(Collections.singletonList(moAvailableAreaTypes)).when(moAvailableAreaTypesRepository).findByAreaTypes(areaTypePrimary1, moId);
        Validation validation = new Validation();
        areaHelper.checkAreaTypeAvailable(111, null, areaTypePrimary1, validation);
        assertEquals(validation.getMessages().size(), 1);
        assertEquals(validation.getMessages().get(0).getMessage(), "Невозможно создать/изменить участок, так как тип участка " + areaTypePrimary1.getTitle() + " отсутствует в списке разрешённых");
        validation.reset();
        assertDoesNotThrow(() -> areaHelper.checkAreaTypeAvailable(moId, null, areaTypePrimary1, validation));
        validation.reset();
        areaHelper.checkAreaTypeAvailable(111, 111L, areaTypeDependent1, validation);
        assertEquals(validation.getMessages().size(), 1);
        assertEquals(validation.getMessages().get(0).getMessage(), "Невозможно создать/изменить участок, так как тип участка " + areaTypeDependent1.getTitle() + " отсутствует в списке разрешённых");
        validation.reset();
        assertDoesNotThrow(() -> areaHelper.checkAreaTypeAvailable(moId, muId, areaTypeDependent1, validation));
    }

    @Test
    void checkAreaTypeCountLimits() {
        doReturn(Collections.singletonList(areaPrimary1)).when(areaRepository).findAreas(moId, null, areaTypePrimary1.getCode(), null, true);
        doReturn(Collections.singletonList(areaDependent1)).when(areaRepository).findAreas(null, muId, areaTypeDependent1.getCode(), null, true);
        Validation validation = new Validation();
        Throwable exception = assertThrows(ContingentException.class, () -> areaHelper.checkAreaTypeCountLimits(moId, null, areaTypePrimary1, validation));
        assertEquals(exception.getMessage(), "Невозможно создать участок, так как количество участков с типом " + areaTypePrimary1.getTitle() + " превысит ограничение (Не более одного на МО)");
        validation.reset();
        assertDoesNotThrow(() -> areaHelper.checkAreaTypeCountLimits(111, null, areaTypePrimary1, validation));
        validation.reset();
        exception = assertThrows(ContingentException.class, () -> areaHelper.checkAreaTypeCountLimits(moId, muId, areaTypeDependent1, validation));
        assertEquals(exception.getMessage(), "Невозможно создать участок, так как количество участков с типом " + areaTypeDependent1.getTitle() + " превысит ограничение (Не более одного на МУ)");
        validation.reset();
        assertDoesNotThrow(() -> areaHelper.checkAreaTypeCountLimits(111, 111L, areaTypeDependent1, validation));
    }

    @Test
    void checkAreaExistsInMU() {
        doReturn(Collections.singletonList(areaPrimary1)).when(areaRepository).findAreas(moId, null, areaTypePrimary1.getCode(), 123, true);
        doReturn(Collections.singletonList(areaDependent1)).when(areaRepository).findAreas(null, muId, areaTypeDependent1.getCode(), 124, true);
        Validation validation = new Validation();
        Throwable exception = assertThrows(ContingentException.class, () -> {
            areaHelper.checkAreaExistsInMU(muId, moId, areaTypeDependent1, 124, null, validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), "Участок обслуживания с типом " + areaTypeDependent1.getTitle() + " и номером 124 уже существует в рамках филиала МО");
        validation.reset();
        assertDoesNotThrow(() -> areaHelper.checkAreaExistsInMU(muId, moId, areaTypePrimary1, 125, null, validation));
        validation.reset();
        exception = assertThrows(ContingentException.class, () -> {
            areaHelper.checkAreaExistsInMU(null, moId, areaTypePrimary1, 123, null, validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), "Участок обслуживания с типом " + areaTypePrimary1.getTitle() + " и номером 123 уже существует в рамках филиала МО");
    }

    @Test
    void checkPolicyTypesIsOMS() {
        Validation validation = new Validation();
        Throwable exception = assertThrows(ContingentException.class, () -> {
            areaHelper.checkPolicyTypesIsOMS(Arrays.asList(4L, 8L), validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), "Невозможно создать/изменить участок, так как добавляемый тип полиса отличен от ОМС");
        validation.reset();
        assertDoesNotThrow(() -> areaHelper.checkPolicyTypesIsOMS(Arrays.asList(1L), validation));
    }

    @Test
    void checkPolicyTypesDel() {
        doReturn(Collections.singletonList(areaPolicyType)).when(areaPolicyTypesRepository).findAll(areaPrimary1, policyType1);
        Validation validation = new Validation();
        Throwable exception = assertThrows(ContingentException.class, () -> {
            areaHelper.checkPolicyTypesDel(areaPrimary1, Arrays.asList(policyType2), validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), "Тип полиса с кодом " + policyType2.getCode() + " не задан для участка с ИД " + areaPrimary1.getId());
        validation.reset();
        assertDoesNotThrow(() -> areaHelper.checkPolicyTypesDel(areaPrimary1, Arrays.asList(policyType1), validation));
    }

    @Test
    void checkAutoAssignForAttachment() {
        Validation validation = new Validation();
        Throwable exception = assertThrows(ContingentException.class, () -> {
            areaHelper.checkAutoAssignForAttachment(areaTypeDependent1, true, true, validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), "Для участка не может быть одновременно установлены признаки «Назначать для автоматического прикрепления» и «Необходимость медицинских показаний»");
        validation.reset();
        exception = assertThrows(ContingentException.class, () -> {
            areaHelper.checkAutoAssignForAttachment(areaTypePrimary1, true, false, validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), "Невозможно установить признак для автоматического прикрепления, т.к. для данного типа участка (Терапевтический) не разрешено прикрепление через МПГУ");
        validation.reset();
        assertDoesNotThrow(() -> areaHelper.checkAutoAssignForAttachment(areaTypePrimary1, false, true, validation));
        assertDoesNotThrow(() -> areaHelper.checkAutoAssignForAttachment(areaTypeDependent1, true, false, validation));
    }

    @Test
    void checkAttachByMedicalReason() {
        Validation validation = new Validation();
        Throwable exception = assertThrows(ContingentException.class, () -> {
            areaHelper.checkAttachByMedicalReason(areaTypePrimary1, false, validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), "Значение признака «Необходимость медицинских показаний» (false) не соответствует настройкам типа участка (true)");
        validation.reset();
        areaTypePrimary1.setAttachByMedicalReason(false);
        exception = assertThrows(ContingentException.class, () -> {
            areaHelper.checkAttachByMedicalReason(areaTypePrimary1, true, validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), "Значение признака «Необходимость медицинских показаний» (true) не соответствует настройкам типа участка (false)");
        validation.reset();
        assertDoesNotThrow(() -> areaHelper.checkAttachByMedicalReason(areaTypePrimary1, true, validation));
        assertDoesNotThrow(() -> areaHelper.checkAttachByMedicalReason(areaTypeDependent1, null, validation));
    }
    
    @Test
    void checkAreaParametersForUpdate() {
        Validation validation = new Validation();
        Throwable exception = assertThrows(ContingentException.class, () -> {
            areaHelper.checkAreaParametersForUpdate(null, null, null, null, null, null, null, null, null, null, null, null, validation);
            throwValidation(validation);
        }); 
        assertEquals(exception.getMessage(), AreaErrorReason.NOTHING_TO_CHANGE.getDescription());
        validation.reset();
        
        assertDoesNotThrow(() -> {
            areaHelper.checkAreaParametersForUpdate(1, null, null, null, null, null, null, null, null, null, null, null, validation);
            throwValidation(validation);
        });
        assertDoesNotThrow(() -> {
            areaHelper.checkAreaParametersForUpdate(null, Collections.singletonList(1L), null, null, null, null, null, null, null, null, null, null, validation);
            throwValidation(validation);
        });
        assertDoesNotThrow(() -> {
            areaHelper.checkAreaParametersForUpdate(null, null, Collections.singletonList(1L), null, null, null, null, null, null, null, null, null, validation);
            throwValidation(validation);
        });
        assertDoesNotThrow(() -> {
            areaHelper.checkAreaParametersForUpdate(null, null, null, 1, null, null, null, null, null, null, null, null, validation);
            throwValidation(validation);
        });
        assertDoesNotThrow(() -> {
            areaHelper.checkAreaParametersForUpdate(null, null, null, null, 1, null, null, null, null, null, null, null, validation);
            throwValidation(validation);
        });
        assertDoesNotThrow(() -> {
            areaHelper.checkAreaParametersForUpdate(null, null, null, null, null, 1, null, null, null, null, null, null, validation);
            throwValidation(validation);
        });
        assertDoesNotThrow(() -> {
            areaHelper.checkAreaParametersForUpdate(null, null, null, null, null, null, 1, null, null, null, null, null, validation);
            throwValidation(validation);
        });
        assertDoesNotThrow(() -> {
            areaHelper.checkAreaParametersForUpdate(null, null, null, null, null, null, null, 1, null, null, null, null, validation);
            throwValidation(validation);
        });
        assertDoesNotThrow(() -> {
            areaHelper.checkAreaParametersForUpdate(null, null, null, null, null, null, null, null, 1, null, null, null, validation);
            throwValidation(validation);
        });
        assertDoesNotThrow(() -> {
            areaHelper.checkAreaParametersForUpdate(null, null, null, null, null, null, null, null, null, true, null, null, validation);
            throwValidation(validation);
        });
        assertDoesNotThrow(() -> {
            areaHelper.checkAreaParametersForUpdate(null, null, null, null, null, null, null, null, null, null, true, null, validation);
            throwValidation(validation);
        });
        assertDoesNotThrow(() -> {
            areaHelper.checkAreaParametersForUpdate(null, null, null, null, null, null, null, null, null, null, null, "", validation);
            throwValidation(validation);
        });
    }
    
    @Test
    void checkAreaParametersForUpdateChanged() {
        doReturn(Collections.singletonList(areaPolicyType)).when(areaPolicyTypesRepository).findAll(areaPrimary1, Arrays.asList(policyType1));
        doReturn(Collections.singletonList(areaPolicyType)).when(areaPolicyTypesRepository).findAll(areaPrimary1, Arrays.asList(policyType1, policyType2));
        Validation validation = new Validation();
        Throwable exception = assertThrows(ContingentException.class, () -> {
            areaHelper.checkAreaParametersForUpdateChanged(
                areaPrimary1, areaPrimary1.getNumber(), 
                Arrays.asList(policyType1), Arrays.asList(policyType2), 
                areaPrimary1.getAgeMin(), areaPrimary1.getAgeMax(), areaPrimary1.getAgeMMin(), areaPrimary1.getAgeMMax(), 
                areaPrimary1.getAgeWMin(), areaPrimary1.getAgeWMax(), areaPrimary1.getAutoAssignForAttach(), areaPrimary1.getAttachByMedicalReason(), 
                areaPrimary1.getDescription(), validation);
            throwValidation(validation);
        });
        assertEquals(exception.getMessage(), AreaErrorReason.NOTHING_TO_CHANGE.getDescription());
        validation.reset();
        
        //есть типы для удаления
        assertDoesNotThrow(() -> {
            areaHelper.checkAreaParametersForUpdateChanged(
                areaPrimary1, areaPrimary1.getNumber(), 
                Arrays.asList(policyType1), Arrays.asList(policyType1, policyType2), 
                areaPrimary1.getAgeMin(), areaPrimary1.getAgeMax(), areaPrimary1.getAgeMMin(), areaPrimary1.getAgeMMax(), 
                areaPrimary1.getAgeWMin(), areaPrimary1.getAgeWMax(), areaPrimary1.getAutoAssignForAttach(), areaPrimary1.getAttachByMedicalReason(), 
                areaPrimary1.getDescription(), validation);
            throwValidation(validation);
        });
        
        //есть типы для добавления
        assertDoesNotThrow(() -> {
            areaHelper.checkAreaParametersForUpdateChanged(
                areaPrimary1, areaPrimary1.getNumber(), 
                Arrays.asList(policyType1, policyType2), Arrays.asList(policyType1), 
                areaPrimary1.getAgeMin(), areaPrimary1.getAgeMax(), areaPrimary1.getAgeMMin(), areaPrimary1.getAgeMMax(), 
                areaPrimary1.getAgeWMin(), areaPrimary1.getAgeWMax(), areaPrimary1.getAutoAssignForAttach(), areaPrimary1.getAttachByMedicalReason(), 
                areaPrimary1.getDescription(), validation);
            throwValidation(validation);
        });
    }

    private void throwValidation(Validation validation) throws ContingentException {
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

    }
}
