package area.service.methods;

import area.service.ESUTestUtil;
import area.service.MockConfiguration;
import area.service.MockEsuService;
import area.service.MockRepositoriesConfiguration;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.service.AreaServiceHelper;
import moscow.ptnl.contingent.area.service.AreaServiceInternal;
import moscow.ptnl.contingent.service.setting.SettingService;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeClass;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKind;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeRelations;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;
import moscow.ptnl.contingent.nsi.repository.AreaTypeRelationsRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.PolicyTypeRepository;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.area.AreaToAreaTypeRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.service.esu.EsuService;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.InOrder;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import javax.persistence.EntityManager;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes= {MockConfiguration.class, MockRepositoriesConfiguration.class})
public class UpdateDependentAreaTest {

    @Autowired
    private EsuService esuService;

    @Autowired
    private SettingService settingService;

    @Autowired
    public AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    @Autowired
    private AreaServiceInternal areaServiceInternal;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private AreaTypeRelationsRepository areaTypeRelationsRepository;

    @Autowired
    private AreaServiceHelper areaServiceHelper;

    @Autowired
    public MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    @Autowired
    public PolicyTypeRepository policyTypeRepository;

    @Autowired
    public AreaToAreaTypeRepository areaToAreaTypeRepository;

    private Long moId = 204L;
    private Integer number = 1234;
    private Long areaTypeDepCode = 10L;
    private Long areaTypePrimCode = 15L;
    private Long areaTypePrim2Code = 16L;
    private Long policyTypeCode = 1L;

    //Тестовые данные
    private AreaType areaTypeDependent;
    private AreaType areaTypePrimary1;
    private AreaType areaTypePrimary2;
    private MoAvailableAreaTypes moAvailableAreaTypes;
    private MoAvailableAreaTypes moAvailableAreaTypes2;
    private AreaTypeRelations areaTypeRelations;
    private PolicyType policyType1;
    private Area areaPrimary1;
    private Area areaDep1;

    @BeforeEach
    public void init() {
        MockEsuService receiveService = (MockEsuService) esuService;
        receiveService.init(); //очищаем коллекцию полученных сообщений
        //нужно для работы интерцептора LogESU
        Mockito.when(settingService.getPar4()).thenReturn(Boolean.TRUE);
        Mockito.when(areaRepository.getEntityManager()).thenReturn(Mockito.mock(EntityManager.class));
        AreaTypeKind areaTypeKind1 = new AreaTypeKind();
        areaTypeKind1.setCode(2L);
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
        areaTypePrimary1 = new AreaType(areaTypePrimCode, "Терапевтический", false);
        areaTypePrimary1.setAreaTypeClass(areaTypeClass1);
        areaTypePrimary1.setAreaTypeKind(areaTypeKind1);
        areaTypePrimary1.setAreaCountLimit(2);
        areaTypePrimary1.setAgeMin(2);
        areaTypePrimary1.setAgeMax(18);
        areaTypePrimary2 = new AreaType(areaTypePrim2Code, "Терапевтический 2", false);
        areaTypePrimary2.setAreaTypeClass(areaTypeClass1);
        areaTypePrimary2.setAreaTypeKind(areaTypeKind1);
        areaTypePrimary2.setAreaCountLimit(2);
        areaTypePrimary2.setAgeMin(0);
        areaTypePrimary2.setAgeMax(24);
        areaTypeDependent = new AreaType(areaTypeDepCode, "Терапевтический Dep", false);
        areaTypeDependent.setAreaTypeClass(areaTypeClass2);
        areaTypeDependent.setAreaTypeKind(areaTypeKind1);
        areaTypeDependent.setAreaCountLimit(2);
        areaTypeDependent.setAgeMin(2);
        areaTypeDependent.setAgeMax(18);
        moAvailableAreaTypes = new MoAvailableAreaTypes();
        moAvailableAreaTypes.setAreaType(areaTypePrimary1);
        moAvailableAreaTypes.setCreateDate(LocalDateTime.now());
        moAvailableAreaTypes.setId(1L);
        moAvailableAreaTypes.setMoId(moId);
        moAvailableAreaTypes2 = new MoAvailableAreaTypes();
        moAvailableAreaTypes2.setAreaType(areaTypePrimary2);
        moAvailableAreaTypes2.setCreateDate(LocalDateTime.now());
        moAvailableAreaTypes2.setId(1L);
        moAvailableAreaTypes2.setMoId(moId);
        areaTypeRelations = new AreaTypeRelations();
        areaTypeRelations.setDependentAreaType(areaTypeDependent);
        areaTypeRelations.setPrimaryAreaType(areaTypePrimary2);
        areaTypeRelations.setArchived(false);
        policyType1 = new PolicyType();
        policyType1.setArchived(false);
        policyType1.setCode(policyTypeCode);
        policyType1.setTitle("Policy type 1");
        areaPrimary1 = new Area(2L, moId, null, areaTypePrimary1, false, LocalDateTime.now());
        areaPrimary1.setNumber(14);
        areaDep1 = new Area(1L, moId, null, areaTypeDependent, false, LocalDateTime.now());
        areaDep1.setNumber(123);
        areaDep1.setPrimaryAreaTypes(new HashSet<>());
        areaDep1.getPrimaryAreaTypes().add(new AreaToAreaType() {{
            setId(1L);
            setAreaType(areaTypePrimary1);
            setArea(areaDep1);
        }});
    }

    @Test
    public void updateDependentAreaCorrect() {
        doReturn(Optional.of(areaTypeDependent)).when(areaTypesCRUDRepository).findById(areaTypeDepCode);
        doReturn(Optional.of(areaTypePrimary1)).when(areaTypesCRUDRepository).findById(areaTypePrimCode);
        doReturn(Optional.of(areaTypePrimary2)).when(areaTypesCRUDRepository).findById(areaTypePrim2Code);
        doReturn(Collections.singletonList(moAvailableAreaTypes)).when(moAvailableAreaTypesRepository).findByAreaTypes(areaTypePrimary1, moId);
        doReturn(Collections.singletonList(moAvailableAreaTypes2)).when(moAvailableAreaTypesRepository).findByAreaTypes(areaTypePrimary2, moId);
        doReturn(Optional.of(areaTypeRelations)).when(areaTypeRelationsRepository).getByDependentAndPrimaryAreaTypes(areaTypeDependent, areaTypePrimary2);
        doReturn(Collections.singletonList(policyType1)).when(policyTypeRepository).findByIds(Collections.singletonList(policyTypeCode));
        doReturn(Optional.of(areaDep1)).when(areaCRUDRepository).findById(areaDep1.getId());
        doReturn(Collections.singletonList(areaPrimary1)).when(areaRepository).findAreas(areaPrimary1.getMoId(), areaPrimary1.getMuId(), Arrays.asList(areaTypePrimCode), null, true);
        doReturn(Collections.singletonList(areaDep1.getPrimaryAreaTypes().iterator().next())).when(areaToAreaTypeRepository).findAreaTypesByAreaAndTypeCode(areaDep1, Collections.singletonList(areaTypePrimCode));
        //create inOrder object passing any mocks that need to be verified in order
        InOrder order = Mockito.inOrder(areaServiceHelper);
        assertDoesNotThrow(() -> areaServiceInternal.updateDependentArea(1L, null, number, "description",
                Arrays.asList(areaTypePrim2Code), Arrays.asList(areaTypePrimCode),
                Arrays.asList(policyTypeCode), Arrays.asList(), 2, 12, null, null, null, null));
        try {
            //Здесь проверяем только факт и порядок вызова функций areaServiceHelper
            //Сами функции проверяются в AreaServiceHelperTest.java
            order.verify(areaServiceHelper).checkAndGetArea(eq(1L), ArgumentMatchers.any(Validation.class));
            order.verify(areaServiceHelper).checkParametersChanged(eq(areaDep1), isNull(), eq(number), any(), eq(Arrays.asList(areaTypePrim2Code)), eq(Arrays.asList(areaTypePrimCode)),
                    eq(Arrays.asList(policyTypeCode)), eq(Arrays.asList()), eq(2), eq(12), isNull(), isNull(), isNull(), isNull(), ArgumentMatchers.any(Validation.class));
            order.verify(areaServiceHelper).checkAreaDependsOnPrimaryAreaType(eq(areaDep1), eq(areaTypePrimary2), ArgumentMatchers.any(Validation.class));
            order.verify(areaServiceHelper).checkAreaTypeIsPrimary(eq(areaTypePrimary2), ArgumentMatchers.any(Validation.class));
            order.verify(areaServiceHelper).checkAreaTypeAvailable(eq(moId), isNull(), eq(areaTypePrimary2), ArgumentMatchers.any(Validation.class));
            order.verify(areaServiceHelper).checkAreaTypeRelations(eq(areaTypeDependent), eq(areaTypePrimary2), ArgumentMatchers.any(Validation.class));
            order.verify(areaServiceHelper).checkPolicyTypesIsOMS(eq(Collections.singletonList(policyTypeCode)), ArgumentMatchers.any(Validation.class));
            order.verify(areaServiceHelper).checkAreaTypeAgeSetups(eq(areaTypeDependent), eq(2), eq(12), isNull(), isNull(), isNull(), isNull(), any(Validation.class));
            order.verify(areaServiceHelper).saveAndDeleteAreaPolicyTypes(eq(areaDep1), eq(Arrays.asList(policyType1)), eq(Arrays.asList()));
            //получаем сообщения
            MockEsuService receiveService = (MockEsuService) esuService;
            MockEsuService.MockMessage msg = receiveService.getMessage();
            assertNotNull(msg, "Сообщение не получено");
            assertEquals("AttachOnAreaChange", msg.getTopicName(), "Не правильное название топика");
            ESUTestUtil.assertEqualsESUXML("esu/updateDependentArea/test1.xml", msg.getMessage());
        } catch (Throwable e) {
            fail(e);
        }
    }
}
