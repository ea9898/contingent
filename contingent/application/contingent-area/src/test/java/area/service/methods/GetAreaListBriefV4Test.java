package area.service.methods;

import area.service.MockConfiguration;
import area.service.PersistenceConfiguration;
import liquibase.Liquibase;
import liquibase.database.Database;
import liquibase.database.DatabaseFactory;
import liquibase.database.jvm.JdbcConnection;
import liquibase.exception.LiquibaseException;
import liquibase.resource.FileSystemResourceAccessor;
import moscow.ptnl.contingent.domain.area.AreaService;
import moscow.ptnl.contingent.domain.area.model.area.AreaHistory;
import moscow.ptnl.contingent.error.ContingentException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.transaction.annotation.Transactional;
import ru.mos.emias.contingent2.area.v4.AreaPT;
import ru.mos.emias.contingent2.area.v4.Fault;
import ru.mos.emias.contingent2.area.v4.types.GetAreaListBriefRequest;
import ru.mos.emias.contingent2.area.v4.types.GetAreaListBriefResponse;
import ru.mos.emias.contingent2.core.v4.KeyValuePair;
import ru.mos.emias.contingent2.core.v4.Options;

import javax.sql.DataSource;
import java.sql.SQLException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes= {PersistenceConfiguration.class, MockConfiguration.class})
@Transactional
public class GetAreaListBriefV4Test {

    private static final PageRequest PR = PageRequest.of(0, 10);

    @Autowired
    private AreaService areaServiceDomain;

    @Autowired
    private ru.mos.emias.contingent2.area.v4.AreaPT areaPTv4;

    @BeforeAll
    public static void init(@Qualifier("contingentDataSource") DataSource dataSource) throws LiquibaseException, SQLException {
        Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(dataSource.getConnection()));
        Liquibase liquibase = new Liquibase("changelog/area/versions/master.xml", new FileSystemResourceAccessor("../database"), database);
        liquibase.update("");
    }

    @Test
    public void getAreaListBriefshowMEIncorrect() {
        GetAreaListBriefRequest request = new GetAreaListBriefRequest();
        request.setAreas(new GetAreaListBriefRequest.Areas() {{ getIds().add(1L); }});
        request.setOptions(new Options() {{ getEntries().add(new KeyValuePair(){{ setKey("showME"); setValue("main-vrio1");}}); }});

        Throwable exception = assertThrows(ru.mos.emias.contingent2.area.v4.Fault.class, () -> areaPTv4.getAreaListBrief(request));
        assertEquals(exception.getMessage(), "Допустимые значения параметра value: all, main, vrio, replacement, main-vrio, none");
    }

    @Test
    public void getAreaListBriefEmptyResult() throws Fault {
        GetAreaListBriefRequest request = new GetAreaListBriefRequest();
        request.setAreas(new GetAreaListBriefRequest.Areas() {{ getIds().add(1L); }});
        request.setOptions(new Options() {{ getEntries().add(new KeyValuePair(){{ setKey("showME"); setValue("replacement");}}); }});

        GetAreaListBriefResponse response = areaPTv4.getAreaListBrief(request);
        assertEquals(0, response.getResult().getAreas().size());
    }

    @Test
    @Sql(scripts = {"/sql/area_type.sql", "/sql/getAreaListBrief2729.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void getAreaListBriefShowMEAll() throws Fault {
        GetAreaListBriefRequest request = new GetAreaListBriefRequest();
        request.setAreas(new GetAreaListBriefRequest.Areas() {{ getIds().add(17411941153L); }});
        request.setOptions(new Options() {{ getEntries().add(new KeyValuePair(){{ setKey("showME"); setValue("all");}}); }});

        GetAreaListBriefResponse response = areaPTv4.getAreaListBrief(request);
        assertEquals(1, response.getResult().getAreas().size());
        assertEquals(141, response.getResult().getAreas().get(0).getMoId());
        assertEquals(4, response.getResult().getAreas().get(0).getMedicalEmployees().getMedicalEmployees().size());
    }

    @Test
    @Sql(scripts = {"/sql/area_type.sql", "/sql/getAreaListBrief2729.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void getAreaListBriefNoShowME() throws Fault {
        GetAreaListBriefRequest request = new GetAreaListBriefRequest();
        request.setAreas(new GetAreaListBriefRequest.Areas() {{ getIds().add(17411941153L); }});

        GetAreaListBriefResponse response = areaPTv4.getAreaListBrief(request);
        assertEquals(1, response.getResult().getAreas().size());
        assertEquals(141, response.getResult().getAreas().get(0).getMoId());
        assertEquals(2, response.getResult().getAreas().get(0).getMedicalEmployees().getMedicalEmployees().size());
    }

    @Test
    @Sql(scripts = {"/sql/area_type.sql", "/sql/getAreaListBrief2729.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void getAreaListBriefShowMEMain() throws Fault {
        GetAreaListBriefRequest request = new GetAreaListBriefRequest();
        request.setAreas(new GetAreaListBriefRequest.Areas() {{ getIds().add(17411941153L); }});
        request.setOptions(new Options() {{ getEntries().add(new KeyValuePair(){{ setKey("showME"); setValue("main");}}); }});

        GetAreaListBriefResponse response = areaPTv4.getAreaListBrief(request);
        assertEquals(1, response.getResult().getAreas().size());
        assertEquals(141, response.getResult().getAreas().get(0).getMoId());
        assertEquals(1, response.getResult().getAreas().get(0).getMedicalEmployees().getMedicalEmployees().size());
        assertEquals(9262292360L, response.getResult().getAreas().get(0).getMedicalEmployees().getMedicalEmployees().get(0).getMedicalEmployeeJobInfoId());
        assertEquals(0, response.getResult().getAreas().get(0).getMedicalEmployees().getMedicalEmployees().get(0).getEmployeeCategory());
    }

    @Test
    @Sql(scripts = {"/sql/area_type.sql", "/sql/getAreaListBrief2729.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void getAreaListBriefShowMEVrio() throws Fault {
        GetAreaListBriefRequest request = new GetAreaListBriefRequest();
        request.setAreas(new GetAreaListBriefRequest.Areas() {{ getIds().add(17411941153L); }});
        request.setOptions(new Options() {{ getEntries().add(new KeyValuePair(){{ setKey("showME"); setValue("vrio");}}); }});

        GetAreaListBriefResponse response = areaPTv4.getAreaListBrief(request);
        assertEquals(1, response.getResult().getAreas().size());
        assertEquals(141, response.getResult().getAreas().get(0).getMoId());
        assertEquals(1, response.getResult().getAreas().get(0).getMedicalEmployees().getMedicalEmployees().size());
        assertEquals(9262292361L, response.getResult().getAreas().get(0).getMedicalEmployees().getMedicalEmployees().get(0).getMedicalEmployeeJobInfoId());
        assertEquals(1, response.getResult().getAreas().get(0).getMedicalEmployees().getMedicalEmployees().get(0).getEmployeeCategory());
    }

    @Test
    @Sql(scripts = {"/sql/area_type.sql", "/sql/getAreaListBrief2729.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void getAreaListBriefShowMEReplacement() throws Fault {
        GetAreaListBriefRequest request = new GetAreaListBriefRequest();
        request.setAreas(new GetAreaListBriefRequest.Areas() {{ getIds().add(17411941153L); }});
        request.setOptions(new Options() {{ getEntries().add(new KeyValuePair(){{ setKey("showME"); setValue("replacement");}}); }});

        GetAreaListBriefResponse response = areaPTv4.getAreaListBrief(request);
        assertEquals(1, response.getResult().getAreas().size());
        assertEquals(141, response.getResult().getAreas().get(0).getMoId());
        assertEquals(2, response.getResult().getAreas().get(0).getMedicalEmployees().getMedicalEmployees().size());
        assertEquals(2, response.getResult().getAreas().get(0).getMedicalEmployees().getMedicalEmployees().get(0).getEmployeeCategory());
        assertEquals(9262292362L, response.getResult().getAreas().get(0).getMedicalEmployees().getMedicalEmployees().get(0).getMedicalEmployeeJobInfoId());
        assertEquals(2, response.getResult().getAreas().get(0).getMedicalEmployees().getMedicalEmployees().get(1).getEmployeeCategory());
        assertEquals(9262292363L, response.getResult().getAreas().get(0).getMedicalEmployees().getMedicalEmployees().get(1).getMedicalEmployeeJobInfoId());
    }

    @Test
    @Sql(scripts = {"/sql/area_type.sql", "/sql/getAreaListBrief2729.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void getAreaListBriefShowMEMainVrio() throws Fault {
        GetAreaListBriefRequest request = new GetAreaListBriefRequest();
        request.setAreas(new GetAreaListBriefRequest.Areas() {{ getIds().add(17411941153L); }});
        request.setOptions(new Options() {{ getEntries().add(new KeyValuePair(){{ setKey("showME"); setValue("main-vrio");}}); }});

        GetAreaListBriefResponse response = areaPTv4.getAreaListBrief(request);
        assertEquals(1, response.getResult().getAreas().size());
        assertEquals(141, response.getResult().getAreas().get(0).getMoId());
        assertEquals(2, response.getResult().getAreas().get(0).getMedicalEmployees().getMedicalEmployees().size());
    }

    @Test
    @Sql(scripts = {"/sql/area_type.sql", "/sql/getAreaListBrief2729.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void getAreaListBriefShowMENone() throws Fault {
        GetAreaListBriefRequest request = new GetAreaListBriefRequest();
        request.setAreas(new GetAreaListBriefRequest.Areas() {{ getIds().add(17411941153L); }});
        request.setOptions(new Options() {{ getEntries().add(new KeyValuePair(){{ setKey("showME"); setValue("none");}}); }});

        GetAreaListBriefResponse response = areaPTv4.getAreaListBrief(request);
        assertEquals(1, response.getResult().getAreas().size());
        assertEquals(141, response.getResult().getAreas().get(0).getMoId());
        assertNull(response.getResult().getAreas().get(0).getMedicalEmployees());
    }

}
