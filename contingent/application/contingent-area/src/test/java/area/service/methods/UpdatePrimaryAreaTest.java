package area.service.methods;

import area.service.ESUTestUtil;
import area.service.MockConfiguration;
import area.service.MockEsuService;
import area.service.MockRepositoriesConfiguration;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.AreaPolicyTypes;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClass;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeKind;
import moscow.ptnl.contingent.area.entity.nsi.PolicyType;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.service.AreaServiceHelper;
import moscow.ptnl.contingent.area.service.AreaServiceInternal;
import moscow.ptnl.contingent.area.service.SettingService;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.repository.nsi.AreaPolicyTypesRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.PolicyTypeRepository;
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
import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;

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
    public AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    @Autowired
    private AreaServiceInternal areaServiceInternal;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private AreaPolicyTypesRepository areaPolicyTypesRepository;

    @Autowired
    private PolicyTypeRepository policyTypeRepository;

    @Autowired
    private AreaServiceHelper areaServiceHelper;

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
        areaPrimary1 = new Area(1L, moId, null, areaTypePrimary1, false, LocalDateTime.now());
        areaPrimary1.setNumber(123);
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
    public void updatePrimaryAreaCorrect() {
        doReturn(Optional.of(areaTypePrimary1)).when(areaTypesCRUDRepository).findById(areaType);
        doReturn(Collections.singletonList(moAvailableAreaTypes)).when(moAvailableAreaTypesRepository).findByAreaTypes(areaTypePrimary1, moId);
        doAnswer(invocation -> Optional.of(areaPrimary1)).when(areaCRUDRepository).findById(1L);
        doReturn(Collections.singletonList(policyType1)).when(policyTypeRepository).findByIds(Collections.singletonList(policyType1.getCode()));
        doReturn(Collections.singletonList(areaPolicyType)).when(areaPolicyTypesRepository).findAll(areaPrimary1, policyType1);
        //create inOrder object passing any mocks that need to be verified in order
        InOrder order = Mockito.inOrder(areaServiceHelper);
        assertDoesNotThrow(() -> areaServiceInternal.updatePrimaryArea(1L, number, Arrays.asList(), Arrays.asList(1L),
                2, 12, null, null, null, null, true, false, "description"));
        try {
            //Здесь проверяем только факт и порядок вызова функций areaServiceHelper
            //Сами функции проверяются в AreaServiceHelperTest.java
            order.verify(areaServiceHelper).checkAndGetArea(eq(1L), ArgumentMatchers.any(Validation.class));
            order.verify(areaServiceHelper).checkAreaExistsInMU(isNull(), eq(moId), eq(areaTypePrimary1), eq(number), eq(areaPrimary1.getId()), any(Validation.class));
            order.verify(areaServiceHelper).checkPolicyTypesIsOMS(eq(Collections.emptyList()), any(Validation.class));
            order.verify(areaServiceHelper).checkPolicyTypesDel(eq(areaPrimary1), eq(Collections.singletonList(policyType1)), any(Validation.class));
            order.verify(areaServiceHelper).checkAutoAssignForAttachment(eq(areaTypePrimary1), eq(true), eq(false), any(Validation.class));
            order.verify(areaServiceHelper).checkAttachByMedicalReason(eq(areaTypePrimary1), eq(false), any(Validation.class));
            order.verify(areaServiceHelper).checkAreaTypeAgeSetups(eq(areaTypePrimary1), eq(2), eq(12), isNull(), isNull(), isNull(), isNull(), any(Validation.class));
            order.verify(areaServiceHelper).resetAutoAssignForAttachment(eq(areaPrimary1));
            order.verify(areaServiceHelper).saveAndDeleteAreaPolicyTypes(eq(areaPrimary1), eq(Arrays.asList()), eq(Arrays.asList(policyType1)));
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
