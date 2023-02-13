package area.service.methods;

import area.service.ESUTestUtil;
import area.service.MockConfiguration;
import area.service.MockEsuService;
import area.service.MockRepositoriesConfiguration;

import moscow.ptnl.contingent.domain.area.AreaService;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.MoAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.heplers.AreaHelper;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeClass;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKind;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeRelations;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypeRelationsRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.nsi.domain.repository.PolicyTypeRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaRepository;
import moscow.ptnl.contingent.domain.area.repository.MoAvailableAreaTypesRepository;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.InOrder;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import jakarta.persistence.EntityManager;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Optional;
import moscow.ptnl.contingent.esu.service.EsuService;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes= {MockConfiguration.class, MockRepositoriesConfiguration.class})
public class CreateDependentAreaTest {

    @Autowired
    private EsuService esuService;

    @Autowired
    private SettingService settingService;

    @Autowired
    public AreaTypesCRUDRepository areaTypesRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private AreaTypeRelationsRepository areaTypeRelationsRepository;

    @MockBean
    private AreaHelper areaHelper;

    @Autowired
    public MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    @Autowired
    public PolicyTypeRepository policyTypeRepository;

    @Autowired
    private AreaService areaServiceDomain;

    private Long moId = 204L;
    private Integer number = 1234;
    private Long areaTypeDepCode = 10L;
    private Long areaTypePrimCode = 15L;
    private Long policyTypeCode = 1L;

    //Тестовые данные
    private AreaType areaTypeDependent;
    private AreaType areaTypePrimary1;
    private MoAvailableAreaTypes moAvailableAreaTypes;
    private AreaTypeRelations areaTypeRelations;
    private PolicyType policyType;
    private Area areaPrimary1;

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
        areaTypeDependent = new AreaType(areaTypeDepCode, "Терапевтический Dep", false);
        areaTypeDependent.setAreaTypeClass(areaTypeClass2);
        areaTypeDependent.setAreaTypeKind(areaTypeKind1);
        areaTypeDependent.setAreaCountLimit(2);
        areaTypeDependent.setAgeMin(2);
        areaTypeDependent.setAgeMax(18);
        moAvailableAreaTypes = new MoAvailableAreaTypes();
        moAvailableAreaTypes.setAreaType(areaTypeDependent);
        moAvailableAreaTypes.setCreateDate(LocalDateTime.now());
        moAvailableAreaTypes.setId(1L);
        moAvailableAreaTypes.setMoId(moId);
        areaTypeRelations = new AreaTypeRelations();
        areaTypeRelations.setDependentAreaType(areaTypeDependent);
        areaTypeRelations.setPrimaryAreaType(areaTypePrimary1);
        areaTypeRelations.setArchived(false);
        policyType = new PolicyType();
        policyType.setArchived(false);
        policyType.setCode(policyTypeCode);
        policyType.setTitle("Policy type 1");
        areaPrimary1 = Area.builder()
                .id(2L)
                .moId(moId)
                .areaType(areaTypePrimary1)
                .archived(false)
                .createDate(LocalDateTime.now())
                .number(123)
                .build();
    }

    @Test
    @Disabled
    public void createDependentAreaCorrect() {
        final Area[] createdArea = new Area[1];
        doReturn(Optional.of(areaTypeDependent)).when(areaTypesRepository).findById(areaTypeDepCode);
        doReturn(Optional.of(areaTypePrimary1)).when(areaTypesRepository).findById(areaTypePrimCode);
        doReturn(Collections.singletonList(moAvailableAreaTypes)).when(moAvailableAreaTypesRepository).findByAreaTypes(areaTypePrimary1, moId);
        doReturn(Optional.of(areaTypeRelations)).when(areaTypeRelationsRepository).getByDependentAndPrimaryAreaTypes(areaTypeDependent, areaTypePrimary1);
        doReturn(Collections.singletonList(policyType)).when(policyTypeRepository).findByIds(Collections.singletonList(policyTypeCode));
        doAnswer(invocation -> {
            Object[] args = invocation.getArguments();
            createdArea[0] = (Area) args[0];
            createdArea[0].setId(1L);
            return args[0];
        }).when(areaRepository).save(any());
        doReturn(Collections.singletonList(areaPrimary1)).when(areaRepository).findAreas(moId, null, Collections.singletonList(areaTypePrimCode), null, true);
        //create inOrder object passing any mocks that need to be verified in order
        InOrder order = Mockito.inOrder(areaHelper);
        Long id = assertDoesNotThrow(() -> areaServiceDomain.createDependentArea(moId, null, number, areaTypeDepCode, Collections.singletonList(areaTypePrimCode),
                Collections.singletonList(policyTypeCode), 2, 12, null, null, null, null, "description"));
        try {
            //Здесь проверяем только факт и порядок вызова функций areaHelper
            //Сами функции проверяются в AreaServiceHelperTest.java
            order.verify(areaHelper).checkAndGetAreaTypesExist(eq(Collections.singletonList(areaTypeDepCode)), ArgumentMatchers.any(Validation.class));
            order.verify(areaHelper).checkAreaTypeIsDependent(eq(areaTypeDependent), ArgumentMatchers.any(Validation.class));
            order.verify(areaHelper).checkAreaTypeIsPrimary(eq(areaTypePrimary1), ArgumentMatchers.any(Validation.class));
            order.verify(areaHelper).checkAreaTypeAvailable(eq(moId), isNull(), eq(areaTypePrimary1), ArgumentMatchers.any(Validation.class));
            order.verify(areaHelper).checkAreaTypeRelations(eq(areaTypeDependent), eq(areaTypePrimary1), ArgumentMatchers.any(Validation.class));
            order.verify(areaHelper).checkPolicyTypesIsOMS(eq(Collections.singletonList(policyTypeCode)), ArgumentMatchers.any(Validation.class));
            order.verify(areaHelper).checkAreaTypeAgeSetups(eq(areaTypeDependent), eq(2), eq(12), isNull(), isNull(), isNull(), isNull(), any(Validation.class));

            assertEquals(id, Long.valueOf(1L));
            //получаем сообщения
            MockEsuService receiveService = (MockEsuService) esuService;
            MockEsuService.MockMessage msg = receiveService.getMessage();
            assertNotNull(msg, "Сообщение не получено");
            assertEquals("AttachOnAreaChange", msg.getTopicName(), "Не правильное название топика");
            ESUTestUtil.assertEqualsESUXML("esu/createDependentArea/test1.xml", msg.getMessage());
        } catch (Throwable e) {
            fail(e);
        }
    }
}
