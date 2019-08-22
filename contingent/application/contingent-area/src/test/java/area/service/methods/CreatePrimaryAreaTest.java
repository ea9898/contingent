package area.service.methods;

import area.service.MockConfiguration;
import area.service.MockEsuService;
import area.service.MockRepositoriesConfiguration;
import moscow.ptnl.contingent.area.service.AreaServiceInternal;
import moscow.ptnl.contingent.area.service.SettingService;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.service.esu.EsuService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import javax.persistence.EntityManager;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes= {MockConfiguration.class, MockRepositoriesConfiguration.class})
public class CreatePrimaryAreaTest {

    @Autowired
    private EsuService esuService;

    @Autowired
    private SettingService settingService;

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    @Autowired
    private AreaServiceInternal areaServiceInternal;

    @Autowired
    private AreaRepository areaRepository;

    @BeforeEach
    public void init() {
        MockEsuService receiveService = (MockEsuService) esuService;
        receiveService.init(); //очищаем коллекцию полученных сообщений
        //нужно для работы интерцептора LogESU
        Mockito.when(settingService.getPar4()).thenReturn(Boolean.TRUE);
        Mockito.when(areaRepository.getEntityManager()).thenReturn(Mockito.mock(EntityManager.class));
    }


}
