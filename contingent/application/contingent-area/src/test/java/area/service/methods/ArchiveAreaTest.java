package area.service.methods;

import area.service.MockConfiguration;
import area.service.MockEsuService;
import area.service.MockRepositoriesConfiguration;
import moscow.ptnl.contingent.area.entity.area.Addresses;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaAddress;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClass;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.service.AreaServiceInternal;
import moscow.ptnl.contingent.area.service.SettingService;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.nsi.PositionCodeRepository;
import moscow.ptnl.contingent.service.esu.EsuService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import javax.persistence.EntityManager;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.HashSet;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doReturn;
import static org.xmlunit.assertj.XmlAssert.assertThat;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes= {MockConfiguration.class, MockRepositoriesConfiguration.class})
public class ArchiveAreaTest {

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    @Autowired
    private AreaServiceInternal areaServiceInternal;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private EsuService esuService;

    @Autowired
    private SettingService settingService;

    private AreaType areaTypePrimary1;
    private AreaType areaTypeDependent1;
    private AreaToAreaType areaToAreaType1;
    private Area areaPrimary1;
    private Area areaDependent1;
    private AreaAddress areaAddress1;
    private AreaMedicalEmployees areaMedicalEmployee1;
    private Long moId = 204L;

    @BeforeEach
    public void init() {
        MockEsuService receiveService = (MockEsuService) esuService;
        receiveService.init(); //очищаем коллекцию полученных сообщений
        //нужно для работы интерцептора LogESU
        Mockito.when(settingService.getPar4()).thenReturn(Boolean.TRUE);
        Mockito.when(areaRepository.getEntityManager()).thenReturn(Mockito.mock(EntityManager.class));
        AreaTypeClass areaTypeClass1 = new AreaTypeClass();
        areaTypeClass1.setCode(1L);
        areaTypeClass1.setTitle("Primary test");
        areaTypeClass1.setArchived(false);
        AreaTypeClass areaTypeClass2 = new AreaTypeClass();
        areaTypeClass2.setCode(10L);
        areaTypeClass2.setTitle("Dependent test");
        areaTypeClass2.setArchived(false);
        areaTypePrimary1 = new AreaType(10L, "Терапевтический", false);
        areaTypePrimary1.setAreaTypeClass(areaTypeClass1);
        areaTypeDependent1 = new AreaType(20L, "Терапевтический 2", false);
        areaTypeDependent1.setAreaTypeClass(areaTypeClass2);
        areaPrimary1 = new Area(1L, moId, null, areaTypePrimary1, false, LocalDateTime.now());
        areaPrimary1.setNumber(123);
        areaDependent1 = new Area(2L, moId, null, areaTypeDependent1, false, LocalDateTime.now());
        areaDependent1.setAgeMin(1);
        areaDependent1.setAgeMax(16);
        areaToAreaType1 = new AreaToAreaType();
        areaToAreaType1.setId(1L);
        areaToAreaType1.setArea(areaDependent1);
        areaToAreaType1.setAreaType(areaTypeDependent1);
        areaAddress1 = new AreaAddress();
        areaAddress1.setArea(areaPrimary1);
        areaAddress1.setAddress(new Addresses());
        areaAddress1.setCreateDate(LocalDateTime.now());
        areaAddress1.setStartDate(LocalDate.now().minusDays(5));
        areaPrimary1.setAreaAddresses(new HashSet<>(Collections.singletonList(areaAddress1)));
        areaMedicalEmployee1 = new AreaMedicalEmployees(111L, areaDependent1, false,
                LocalDate.now().minusDays(5), null, "", Optional.empty(), LocalDateTime.now(), null, 1L);
        areaDependent1.setMedicalEmployees(new HashSet<>(Collections.singletonList(areaMedicalEmployee1)));
    }

    @Test
    public void archiveAreaExceptionTest() {
        Throwable exception = assertThrows(ContingentException.class, () -> areaServiceInternal.archiveArea(1111L));
        assertEquals(exception.getMessage(), "Участок обслуживания МО с ИД 1111 не найден в системе");
    }

    @Test
    public void archivePrimaryAreaWithEsuTest() {
        archiveAreaWithEsuTest(areaPrimary1);
    }

    @Test
    public void archiveDependentAreaWithEsuTest() {
        archiveAreaWithEsuTest(areaDependent1);
    }

    private void archiveAreaWithEsuTest(Area areaTest) {
        doReturn(Optional.of(areaTest)).when(areaCRUDRepository).findById(areaTest.getId());

        try {
            areaServiceInternal.archiveArea(areaTest.getId());

            assertEquals(areaTest.getArchived(), Boolean.TRUE);
            assertEquals(areaTest.getActualAreaAddresses().size(), 0);
            assertEquals(areaTest.getActualMedicalEmployees().size(), 0);

            MockEsuService receiveService = (MockEsuService) esuService;
            MockEsuService.MockMessage msg = receiveService.getMessage();

            if (areaTest.getAreaType().getCode() == 1) { //основной участок
                assertNotNull(msg, "Сообщение не получено");
                assertEquals("AreaInfo", msg.getTopicName(), "Не правильное название топика");
                assertThat(msg.getMessage()).hasXPath(".//id");
                assertThat(msg.getMessage()).valueByXPath(".//id/text()").isEqualTo("1");
            }
            else if (areaTest.getAreaType().getCode() == 2) { //зависимый участок
                assertNull(msg, "Для зависимого участка сообщение не должно отправляться");
            }
        } catch (ContingentException e) {
            fail();
        }
    }
}
