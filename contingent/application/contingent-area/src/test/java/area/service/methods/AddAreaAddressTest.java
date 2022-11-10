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
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import moscow.ptnl.contingent.error.ContingentException;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.transaction.annotation.Transactional;
import ru.mos.emias.contingent2.address.v3.AddressRegistryBaseType;
import ru.mos.emias.contingent2.address.v3.AreaOMKTE;
import ru.mos.emias.contingent2.address.v3.Building;
import ru.mos.emias.contingent2.address.v3.City;
import ru.mos.emias.contingent2.address.v3.Names;
import ru.mos.emias.contingent2.address.v3.Place;
import ru.mos.emias.contingent2.address.v3.Plan;
import ru.mos.emias.contingent2.address.v3.Area;
import ru.mos.emias.contingent2.address.v3.RegionOMKTE;
import ru.mos.emias.contingent2.address.v3.Street;
import ru.mos.emias.contingent2.area.v3.AreaPT;
import ru.mos.emias.contingent2.area.v3.types.AddAreaAddressRequest;
import ru.mos.emias.contingent2.area.v3.types.AddAreaAddressResponse;

import javax.sql.DataSource;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = {PersistenceConfiguration.class, MockConfiguration.class})
@Transactional
public class AddAreaAddressTest {

    @Autowired
    private AreaService areaServiceDomain;

    @Autowired
    private AreaPT areaPTv3;

    @BeforeAll
    public static void init(@Qualifier("contingentDataSource") DataSource dataSource) throws LiquibaseException, SQLException {
        Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(dataSource.getConnection()));
        Liquibase liquibase = new Liquibase("changelog/area/versions/master.xml", new FileSystemResourceAccessor("../database"), database);
        liquibase.update("");
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressTest2456() {
        List<AddressRegistry> addressRegistries = new ArrayList<>();
        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(-999999998L);
        addressRegistries.add(addressRegistry);

        Throwable exception = assertThrows(ContingentException.class, () -> areaServiceDomain.addAreaAddress(-999L,
                addressRegistries, false));
        assertEquals(exception.getMessage(), "Участок обслуживания МО с ИД -999 не найден в системе");
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/addAreaAddressTest2456.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressTest2456_1() {
        AddressRegistryBaseType addressRegistry = new AddressRegistryBaseType();
        addressRegistry.setGlobalIdNsi(-999999998L);
        addressRegistry.setAddressString("город Москва, улица Ивантеевская");
        addressRegistry.setAoLevel("8");
        addressRegistry.setRegionOMKTE(new RegionOMKTE() {{ setId(67200856L); setCode("0400"); setName("Москва");}});
        addressRegistry.setArea(new Area() {{
            setId(672008111L);
            setCode("888");
            setName("Сведения о районе в регионе");
            setType(new Names(){{ setFull("Тип"); setShort("Шорт");}});
            setCodeOMKTE("0001");
            setCodeBTI("0002");
            }});
        addressRegistry.setAreaOMKTE(new AreaOMKTE() {{
            setId(672008222L);
            setCode("0403");
            setName("Сведения об округе (по ОМК ТЕ)");
            setType(new Names(){{ setFull("Тип"); setShort("Шорт");}});
        }});
        addressRegistry.setCity(new City() {{
            setId(672008333L);
            setCode("000");
            setName("Город");
            setType(new Names(){{ setFull("Тип"); setShort("Шорт");}});
            setCodeOMKTM("001");
            setCodeBTI("002");
        }});
        addressRegistry.setPlace(new Place() {{
            setId(672008444L);
            setCode("000");
            setName("Сведения о населенном пункте");
            setType(new Names(){{ setFull("Населенный пункт"); setShort("НП");}});
            setCodeOMKTM("111");
            setCodeBTI("222");
        }});
        addressRegistry.setPlan(new Plan() {{
            setId(672008555L);
            setCode("0000");
            setName("Сведения о планировочной структуре");
            setType(new Names(){{ setFull("планировочная структура"); setShort("ПС");}});
            setCodeBTI("195411");
        }});
        addressRegistry.setStreet(new Street() {{
            setId(672008666L);
            setCode("7041");
            setName("Ивантеевская улица");
            setType(new Names(){{ setFull("Улица"); setShort("ул");}});
            setCodeOMKUM("020930");
            setCodeBTI("194863");
        }});
        addressRegistry.setBuilding(new Building(){{
            setHouse(new House() {{ setName("99");}});
        }});

        AddAreaAddressRequest addAreaAddressRequest = new AddAreaAddressRequest();
        addAreaAddressRequest.setAreaId(175715882L);
        addAreaAddressRequest.getAddresses().add(addressRegistry);

        AddAreaAddressResponse response =  assertDoesNotThrow(() -> areaPTv3.addAreaAddress(addAreaAddressRequest));
        assertEquals(1, response.getAreaAddressIds().size());
    }
}
