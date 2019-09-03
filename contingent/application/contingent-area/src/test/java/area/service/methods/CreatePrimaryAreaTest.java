package area.service.methods;

import area.service.ESUTestUtil;
import area.service.MockConfiguration;
import area.service.MockEsuService;
import area.service.MockRepositoriesConfiguration;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeClass;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKind;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.area.service.AreaServiceHelper;
import moscow.ptnl.contingent.area.service.AreaServiceInternal;
import moscow.ptnl.contingent.area.service.SettingService;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.service.esu.EsuService;
import net.bytebuddy.matcher.ElementMatchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.AdditionalAnswers;
import org.mockito.ArgumentMatchers;
import org.mockito.InOrder;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import javax.persistence.EntityManager;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes= {MockConfiguration.class, MockRepositoriesConfiguration.class})
public class CreatePrimaryAreaTest {

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
    private AreaServiceHelper areaServiceHelper;

    @Autowired
    public MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    private Long moId = 204L;
    private Long muId = 111L;
    private Integer number = 1234;
    private Long areaType = 10L;
    private Long policyType = 1L;

    //Тестовые данные
    private AreaType areaTypePrimary1;
    private MoAvailableAreaTypes moAvailableAreaTypes;

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
    }

    @Test
    public void createPrimaryAreaCorrect() {
        final Area[] createdArea = new Area[1];
        doReturn(Optional.of(areaTypePrimary1)).when(areaTypesCRUDRepository).findById(areaType);
        doReturn(Collections.singletonList(moAvailableAreaTypes)).when(moAvailableAreaTypesRepository).findByAreaTypes(areaTypePrimary1, moId);
        doAnswer(invocation -> {
            Object[] args = invocation.getArguments();
            createdArea[0] = (Area) args[0];
            createdArea[0].setId(1L);
            return args[0];
        }).when(areaCRUDRepository).save(any());
        doAnswer(invocation -> Optional.of(createdArea[0])).when(areaCRUDRepository).findById(1L);
        //create inOrder object passing any mocks that need to be verified in order
        InOrder order = Mockito.inOrder(areaServiceHelper);
        Long id = assertDoesNotThrow(() -> areaServiceInternal.createPrimaryArea(moId, null, number, areaType, Collections.singletonList(policyType),
                2, 12, null, null, null, null, true, false, "description"));
        try {
            //Здесь проверяем только факт и порядок вызова функций areaServiceHelper
            //Сами функции проверяются в AreaServiceHelperTest.java
            order.verify(areaServiceHelper).checkAndGetAreaTypesExist(eq(Collections.singletonList(areaType)), ArgumentMatchers.any(Validation.class));
            order.verify(areaServiceHelper).checkAreaTypeIsPrimary(eq(areaTypePrimary1), ArgumentMatchers.any(Validation.class));
            order.verify(areaServiceHelper).checkEmptyMuId(null, areaTypePrimary1);
            order.verify(areaServiceHelper).checkAreaTypeAvailable(eq(moId), isNull(), eq(areaTypePrimary1), any(Validation.class));
            order.verify(areaServiceHelper).checkAreaTypeCountLimits(eq(moId), isNull(), eq(areaTypePrimary1), any(Validation.class));
            order.verify(areaServiceHelper).checkAreaExistsInMU(isNull(), eq(moId), eq(areaTypePrimary1), eq(number), isNull(), any(Validation.class));
            order.verify(areaServiceHelper).checkPolicyTypesIsOMS(eq(Collections.singletonList(policyType)), any(Validation.class));
            order.verify(areaServiceHelper).checkAreaTypeAgeSetups(eq(areaTypePrimary1), eq(2), eq(12), isNull(), isNull(), isNull(), isNull(), any(Validation.class));
            order.verify(areaServiceHelper).checkAutoAssignForAttachment(eq(areaTypePrimary1), eq(true), eq(false), any(Validation.class));
            order.verify(areaServiceHelper).checkAttachByMedicalReason(eq(areaTypePrimary1), eq(false), any(Validation.class));

            assertEquals(id, Long.valueOf(1L));
            //получаем сообщения
            MockEsuService receiveService = (MockEsuService) esuService;
            MockEsuService.MockMessage msg = receiveService.getMessage();
            assertNotNull(msg, "Сообщение не получено");
            assertEquals("AreaInfo", msg.getTopicName(), "Не правильное название топика");
            ESUTestUtil.assertEqualsESUXML("esu/createPrimaryArea/test1.xml", msg.getMessage());
        } catch (Throwable e) {
            fail(e);
        }
    }
}
