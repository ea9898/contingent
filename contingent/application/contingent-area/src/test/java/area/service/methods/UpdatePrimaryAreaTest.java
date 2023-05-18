package area.service.methods;

import area.service.ESUTestUtil;
import area.service.MockConfiguration;
import area.service.MockEsuService;
import area.service.MockRepositoriesConfiguration;
import moscow.ptnl.contingent.domain.area.AreaService;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.MoAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.entity.AreaPolicyTypes;
import moscow.ptnl.contingent.domain.area.heplers.AreaHelper;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeClass;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKind;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypesRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaRepository;
import moscow.ptnl.contingent.domain.area.repository.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaPolicyTypesRepository;
import moscow.ptnl.contingent.nsi.domain.repository.PolicyTypeRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.InOrder;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import jakarta.persistence.EntityManager;
import java.time.LocalDateTime;
import java.util.Arrays;
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
public class UpdatePrimaryAreaTest {

    @Autowired
    private EsuService esuService;

    @Autowired
    private SettingService settingService;

    @Autowired
    public AreaTypesRepository areaTypesRepository;

    @Autowired
    private AreaService areaServiceDomain;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private AreaPolicyTypesRepository areaPolicyTypesRepository;

    @Autowired
    private PolicyTypeRepository policyTypeRepository;

    @Autowired
    private AreaHelper areaHelper;

    @Autowired
    public MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    private Long moId = 204L;
    private Integer number = 1234;
    private Long areaType = 10L;

    //Тестовые данные
    private AreaType areaTypePrimary1;
    private MoAvailableAreaTypes moAvailableAreaTypes;
    private Area areaPrimary1;
    private PolicyType policyType1;
    private AreaPolicyTypes areaPolicyType;

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
        areaTypePrimary1 = new AreaType(areaType, "Терапевтический", false);
        areaTypePrimary1.setAreaTypeClass(areaTypeClass1);
        areaTypePrimary1.setAreaTypeKind(areaTypeKind1);
        areaTypePrimary1.setAreaCountLimit(2);
        areaTypePrimary1.setAgeMin(2);
        areaTypePrimary1.setAgeMax(18);
        moAvailableAreaTypes = new MoAvailableAreaTypes();
        moAvailableAreaTypes.setAreaType(areaTypePrimary1);
        moAvailableAreaTypes.setCreateDate(LocalDateTime.now());
        moAvailableAreaTypes.setId(1L);
        moAvailableAreaTypes.setMoId(moId);
        areaPrimary1 = Area.builder()
                .id(1L)
                .moId(moId)
                .areaType(areaTypePrimary1)
                .archived(false)
                .createDate(LocalDateTime.now())
                .number(123)
                .build();
        policyType1 = new PolicyType();
        policyType1.setCode(1L);
        policyType1.setTitle("Test policy type");
        policyType1.setArchived(false);
        areaPolicyType = new AreaPolicyTypes();
        areaPolicyType.setId(1L);
        areaPolicyType.setArea(areaPrimary1);
        areaPolicyType.setPolicyType(policyType1);
    }

    @Test
    @Disabled
    public void updatePrimaryAreaCorrect() {
        doReturn(Optional.of(areaTypePrimary1)).when(areaTypesRepository).findById(areaType);
        doReturn(Collections.singletonList(moAvailableAreaTypes)).when(moAvailableAreaTypesRepository).findByAreaTypes(areaTypePrimary1, moId);
        doAnswer(invocation -> Optional.of(areaPrimary1)).when(areaRepository).findById(1L);
        doReturn(Collections.singletonList(policyType1)).when(policyTypeRepository).findByIds(Collections.singletonList(policyType1.getCode()));
        doReturn(Collections.singletonList(areaPolicyType)).when(areaPolicyTypesRepository).findAll(areaPrimary1, policyType1);
        //create inOrder object passing any mocks that need to be verified in order
        InOrder order = Mockito.inOrder(areaHelper);
        assertDoesNotThrow(() -> areaServiceDomain.updatePrimaryArea(1L, number, Arrays.asList(), Arrays.asList(1L),
                2, 12, null, null, null, null, true, false, "description"));
        try {
            //Здесь проверяем только факт и порядок вызова функций areaHelper
            //Сами функции проверяются в AreaServiceHelperTest.java
            order.verify(areaHelper).checkAndGetArea(eq(1L), ArgumentMatchers.any(Validation.class), true);
            order.verify(areaHelper).checkAreaParametersForUpdate(eq(number), eq(Arrays.asList()), eq(Arrays.asList(1L)), eq(2), eq(12), isNull(), isNull(), isNull(), isNull(), eq(true), eq(false), eq("description"), any(Validation.class));
            order.verify(areaHelper).checkAreaParametersForUpdateChanged(eq(areaPrimary1), eq(number), eq(Collections.EMPTY_LIST), eq(Collections.singletonList(policyType1)), eq(2), eq(12), isNull(), isNull(), isNull(), isNull(), eq(true), eq(false), eq("description"), any(Validation.class));
            order.verify(areaHelper).checkAreaExistsInMU(isNull(), eq(moId), eq(areaTypePrimary1), eq(number), eq(areaPrimary1.getId()), any(Validation.class));
            order.verify(areaHelper).checkPolicyTypesIsOMS(eq(Collections.emptyList()), any(Validation.class));
            order.verify(areaHelper).checkPolicyTypesDel(eq(areaPrimary1), eq(Collections.singletonList(policyType1)), any(Validation.class));
            order.verify(areaHelper).checkAutoAssignForAttachment(eq(areaTypePrimary1), null, eq(true), eq(false), any(Validation.class));
            order.verify(areaHelper).checkAttachByMedicalReason(eq(areaTypePrimary1), eq(false), any(Validation.class));
            order.verify(areaHelper).checkAreaTypeAgeSetups(eq(areaTypePrimary1), eq(2), eq(12), isNull(), isNull(), isNull(), isNull(), any(Validation.class));
//            order.verify(areaHelper).resetAutoAssignForAttachment(eq(areaPrimary1));
            order.verify(areaHelper).saveAndDeleteAreaPolicyTypes(eq(areaPrimary1), eq(Arrays.asList()), eq(Arrays.asList(policyType1)));
            //получаем сообщения
            MockEsuService receiveService = (MockEsuService) esuService;
            MockEsuService.MockMessage msg = receiveService.getMessage();
            assertNotNull(msg, "Сообщение не получено");
            assertEquals("AreaInfo", msg.getTopicName(), "Не правильное название топика");
            ESUTestUtil.assertEqualsESUXML("esu/updatePrimaryArea/test1.xml", msg.getMessage());
        } catch (Throwable e) {
            fail(e);
        }
    }
    
}
