package area.service.methods;

import area.service.ESUTestUtil;
import area.service.MockConfiguration;
import area.service.MockEsuService;
import area.service.MockEsuService.MockMessage;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import javax.persistence.EntityManager;

import area.service.MockRepositoriesConfiguration;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClass;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.service.AreaServiceInternal;
import moscow.ptnl.contingent.area.service.SettingService;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.service.esu.EsuService;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

/**
 * Прототип теста для методов отправляющих сообщения в ЕСУ.
 * 
 * @author m.kachalov
 */
@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes= {MockConfiguration.class, MockRepositoriesConfiguration.class})
public class RestoreAreaTest {
        
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
    
    
    @BeforeEach
    public void init() {
        MockEsuService reciveService = (MockEsuService) esuService;
        reciveService.init(); //очищаем коллекцию полученных сообщений
    }   
    
    @Test
    public void restoreAreaTestMainAreaType() {
        
        //TODO нужны какие то вменяемые данные
        Area area = new Area();
        area.setId(0L);
        area.setArchived(Boolean.TRUE);
        area.setNumber(0);            
        AreaType areaType = new AreaType();
        areaType.setCode(0L);
        AreaTypeClass areaTypeClass = new AreaTypeClass();
        areaTypeClass.setCode(1L); //основной участок
        areaType.setAreaTypeClass(areaTypeClass);
        area.setAreaType(areaType);
        area.setMuId(1L); 
        area.setMoId(1L);
        
        List<Area> findedPrimaryAreas = new ArrayList<>();
        findedPrimaryAreas.add(new Area(1L, null, null, null, false, LocalDateTime.now()));
            
        try {
            
            //нужно для прохождения метода restoreArea
            Mockito.when(areaCRUDRepository.findById(area.getId())).thenReturn(Optional.of(area)); 
            Mockito.when(areaCRUDRepository.save(area)).thenReturn(area);
            Mockito.when(areaRepository.findAreas(null, area.getMoId(), areaType.getCode(), area.getNumber(), true)).thenReturn(new ArrayList<>());           
            Mockito.when(areaRepository.findPrimaryAreasByAreaEqAreaType(area)).thenReturn(findedPrimaryAreas);
            
            //нужно для работы интерцептора LogESU
            Mockito.when(settingService.getPar4()).thenReturn(Boolean.TRUE); 
            Mockito.when(areaRepository.getEntityManager()).thenReturn(Mockito.mock(EntityManager.class));
            
            //тестируемый метод в ктором есть интерцептор LogESU и внутренняя отправка сообщений в ЕСУ в самом методе
            areaServiceInternal.restoreArea(area.getId());
            
            //получаем сообщения
            MockEsuService reciveService = (MockEsuService) esuService;
            
            MockMessage msg = reciveService.getMessage();
            assertNotNull(msg, "Сообщение не получено");
            //System.out.println(msg.getMessage());
            
            //проверка сообщений на что то, что надо проверить
            
            assertEquals("AreaInfo", msg.getTopicName(), "Не правильное название топика");
            ESUTestUtil.assertEqualsESUXML("esu/restoreArea/test2.xml", msg.getMessage());             

            //assertThat(msg.getMessage()).hasXPath(".//id");
            //assertThat(msg.getMessage()).valueByXPath(".//id/text()").isEqualTo("1");
            
            
        } catch (ContingentException e) {
            assertTrue(false, e.getMessage()); //тут можно тестить проверку
        } catch (Throwable e) {
            fail("Ошибка выполнения теста", e);
            e.printStackTrace();
        }
    }
    
    @Test
    public void restoreAreaTestDependentAreaType() {
        
        //TODO нужны какие то вменяемые данные
        Area area = new Area();
        area.setId(0L);
        area.setArchived(Boolean.TRUE);
        area.setNumber(0);            
        AreaType areaType = new AreaType();
        areaType.setCode(0L);
        AreaTypeClass areaTypeClass = new AreaTypeClass();
        areaTypeClass.setCode(2L); //зависмый участок
        areaType.setAreaTypeClass(areaTypeClass);
        area.setAreaType(areaType);
        area.setMuId(1L); 
        area.setMoId(1L);
        
        List<Area> findedPrimaryAreas = new ArrayList<>();
        findedPrimaryAreas.add(new Area(1L, null, null, null, false, LocalDateTime.now()));
            
        try {
            
            //нужно для прохождения метода restoreArea
            Mockito.when(areaCRUDRepository.findById(area.getId())).thenReturn(Optional.of(area)); 
            Mockito.when(areaCRUDRepository.save(area)).thenReturn(area);
            Mockito.when(areaRepository.findAreas(null, area.getMoId(), areaType.getCode(), area.getNumber(), true)).thenReturn(new ArrayList<>());           
            Mockito.when(areaRepository.findPrimaryAreasByAreaEqAreaType(area)).thenReturn(findedPrimaryAreas);
            
            //нужно для работы интерцептора LogESU
            Mockito.when(settingService.getPar4()).thenReturn(Boolean.TRUE); 
            Mockito.when(areaRepository.getEntityManager()).thenReturn(Mockito.mock(EntityManager.class));
            
            //тестируемый метод в ктором есть интерцептор LogESU и внутренняя отправка сообщений в ЕСУ в самом методе
            areaServiceInternal.restoreArea(area.getId());
            
            //получаем сообщения
            MockEsuService reciveService = (MockEsuService) esuService;
            
            MockMessage msg = reciveService.getMessage();
            assertNotNull(msg, "Сообщение не получено");
            //System.out.println(msg.getMessage());
            
            //проверка сообщений на что то, что надо проверить
            
            assertEquals("AttachOnAreaChange", msg.getTopicName(), "Не правильное название топика");
            ESUTestUtil.assertEqualsESUXML("esu/restoreArea/test1.xml", msg.getMessage());
                
            //assertThat(msg.getMessage()).hasXPath(".//id");
            //assertThat(msg.getMessage()).valueByXPath(".//id/text()").isEqualTo("1");
            
        } catch (ContingentException e) {
            assertTrue(false, e.getMessage()); //тут можно тестить проверку
        } catch (Throwable e) {
            fail("Ошибка выполнения теста", e);
            e.printStackTrace();
        }
    }
    
    
    
    
}