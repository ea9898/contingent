package area.service.methods;

//@RunWith(SpringJUnit4ClassRunner.class)

import area.service.MockConfiguration;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClass;
import moscow.ptnl.contingent.area.service.AreaServiceInternal;
import moscow.ptnl.contingent.area.service.EsuHelperService;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

//@SpringApplicationConfiguration(classes = MocksApplication.class)
//@ContextConfiguration(classes = MocksApplication.class)
//@SpringBootTest
@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes= {MockConfiguration.class})
public class RestoreAreaTest  /*extends BaseTest*/ {
    
    private static final Logger LOG = LoggerFactory.getLogger(RestoreAreaTest.class);
            
    
                
    //@Mock
    //private EsuHelperService esuHelperService;
        
    //@Autowired
    //private AreaServiceInternal areaService;
        
    @Autowired
    private ApplicationContext context;
    
    @Autowired
    private EsuHelperService esuHelperService;
    
    @Autowired
    private AreaServiceInternal areaServiceInternal;
    
    @Autowired
    private AreaRepository areaRepository;
       
    
    private Area area;
    
    @BeforeEach
    public void init() {
        area = new Area();
    }   
    
   /* 
    // (К_УУ_17) Восстановление архивного участка обслуживания
    @LogESU(type = AreaInfoEvent.class, parameters = {"areaId"})
    public void restoreArea(Long areaId) throws ContingentException {
        Validation validation = new Validation();

        // 1. 2.
        Area area = areaHelper.checkAndGetArchivedArea(areaId, validation);

        if (area != null) {
            // 3.
            areaHelper.checkAreaTypeIsNotPersonal(area.getAreaType(), validation);

            // 4.
            areaHelper.checkAreaExistsInMU(area.getMuId(), area.getMoId(), area.getAreaType(), area.getNumber(), area.getId(), validation);
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 5.
        area.setAutoAssignForAttach(false);

        // 6.
        area.setArchived(false);
        areaCRUDRepository.save(area);

        // 8.
        if (areaHelper.isAreaDependent(area)) {
            List<Area> areas = areaRepository.findPrimaryAreasByAreaEqAreaType(area);
                                    
            if (!areas.isEmpty()) {
                esuHelperService.sendAttachOnAreaChangeEvent(
                        areas.stream().map(Area::getId).collect(Collectors.toList()),
                        null, 
                        area
                );
            }
        }

        // 9.
        //if (areaHelper.isAreaPrimary(area)) {
        //    esuHelperService.sendAreaInfoEvent(area, "restoreArea");
        //}

        // 10.
        return;
    }
*/
    
    
    
    @Test
    public void restoreAreaTest() {
        System.out.println("Тест restoreArea");
        esuHelperService.toString();
        try {
            area.setId(0L);
            area.setArchived(Boolean.TRUE);
            area.setNumber(0);            
            AreaType areaType = new AreaType();
            areaType.setCode(0L);
            AreaTypeClass areaTypeClass = new AreaTypeClass();
            areaTypeClass.setCode(2L);
            areaType.setAreaTypeClass(areaTypeClass);
            area.setAreaType(areaType);
            area.setMuId(1L); 
            area.setMoId(1L);
            Mockito.when(context.getBean(AreaCRUDRepository.class).findById(area.getId())).thenReturn(Optional.of(area));   
            Mockito.when(areaRepository.findAreas(null, area.getMoId(), areaType.getCode(), area.getNumber(), true)).thenReturn(new ArrayList<>());
            Mockito.when(context.getBean(AreaCRUDRepository.class).save(area)).thenReturn(area);
            List<Area> areas = new ArrayList<>();
            areas.add(new Area(1L, null, null, null, false, LocalDateTime.now()));
            Mockito.when(areaRepository.findPrimaryAreasByAreaEqAreaType(area)).thenReturn(areas);
            //Mockito.when(areaHelper.isAreaDependent(area)).thenReturn(Boolean.TRUE);
            
            //Mockito.doNothing().when(areaHelper).checkAreaExistsInMU(any(Long.class), any(Integer.class), areaType, any(Integer.class), any(Long.class), validation);
            areaServiceInternal.restoreArea(area.getId());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
}