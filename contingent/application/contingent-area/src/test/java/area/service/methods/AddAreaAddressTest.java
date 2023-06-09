package area.service.methods;

import area.service.MockConfiguration;
import area.service.MockEsuService;
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
import moscow.ptnl.contingent.repository.esu.EsuInputCRUDRepository;
import moscow.ptnl.contingent.repository.esu.EsuOutputCRUDRepository;
import org.junit.jupiter.api.Assertions;
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
import ru.mos.emias.contingent2.area.v3.Fault;
import ru.mos.emias.contingent2.area.v3.AreaPT;
import ru.mos.emias.contingent2.area.v3.types.AddAreaAddressRequest;
import ru.mos.emias.contingent2.area.v3.types.AddAreaAddressResponse;

import javax.sql.DataSource;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Unmarshaller;
import jakarta.xml.soap.MessageFactory;
import jakarta.xml.soap.SOAPConstants;
import jakarta.xml.soap.SOAPException;
import jakarta.xml.soap.SOAPMessage;
import java.io.IOException;
import java.io.InputStream;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import moscow.ptnl.contingent.esu.service.EsuService;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = {PersistenceConfiguration.class, MockConfiguration.class})
@Transactional
public class AddAreaAddressTest {

    @Autowired
    private AreaService areaServiceDomain;

    @Autowired
    private AreaPT areaPTv3;

    @Autowired
    private EsuOutputCRUDRepository esuOutputRepository;

    @Autowired
    private EsuInputCRUDRepository esuInputRepository;

    @Autowired
    private EsuService esuService;

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
        addressRegistry.setRegionOMKTE(new RegionOMKTE() {{
            setId(67200856L);
            setCode("0400");
            setName("Москва");
        }});
        addressRegistry.setArea(new Area() {{
            setId(672008111L);
            setCode("888");
            setName("Сведения о районе в регионе");
            setType(new Names() {{
                setFull("Тип");
                setShort("Шорт");
            }});
            setCodeOMKTE("0001");
            setCodeBTI("0002");
        }});
        addressRegistry.setAreaOMKTE(new AreaOMKTE() {{
            setId(672008222L);
            setCode("0403");
            setName("Сведения об округе (по ОМК ТЕ)");
            setType(new Names() {{
                setFull("Тип");
                setShort("Шорт");
            }});
        }});
        addressRegistry.setCity(new City() {{
            setId(672008333L);
            setCode("000");
            setName("Город");
            setType(new Names() {{
                setFull("Тип");
                setShort("Шорт");
            }});
            setCodeOMKTM("001");
            setCodeBTI("002");
        }});
        addressRegistry.setPlace(new Place() {{
            setId(672008444L);
            setCode("000");
            setName("Сведения о населенном пункте");
            setType(new Names() {{
                setFull("Населенный пункт");
                setShort("НП");
            }});
            setCodeOMKTM("111");
            setCodeBTI("222");
        }});
        addressRegistry.setPlan(new Plan() {{
            setId(672008555L);
            setCode("0000");
            setName("Сведения о планировочной структуре");
            setType(new Names() {{
                setFull("планировочная структура");
                setShort("ПС");
            }});
            setCodeBTI("195411");
        }});
        addressRegistry.setStreet(new Street() {{
            setId(672008666L);
            setCode("7041");
            setName("Ивантеевская улица");
            setType(new Names() {{
                setFull("Улица");
                setShort("ул");
            }});
            setCodeOMKUM("020930");
            setCodeBTI("194863");
        }});
        addressRegistry.setBuilding(new Building() {{
            setHouse(new House() {{
                setName("99");
            }});
        }});

        AddAreaAddressRequest addAreaAddressRequest = new AddAreaAddressRequest();
        addAreaAddressRequest.setAreaId(175715882L);
        addAreaAddressRequest.getAddresses().add(addressRegistry);

        AddAreaAddressResponse response = assertDoesNotThrow(() -> areaPTv3.addAreaAddress(addAreaAddressRequest));
        assertEquals(1, response.getAreaAddressIds().size());
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/addAreaAddress2501.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressTest2501_1() {
        AddressRegistryBaseType addressRegistry = new AddressRegistryBaseType();
        addressRegistry.setGlobalIdNsi(-300L);
        addressRegistry.setAddressString("город Москва, улица Ивантеевская");
        addressRegistry.setAoLevel("8");
        addressRegistry.setRegionOMKTE(new RegionOMKTE() {{
            setId(67200856L);
            setCode("0400");
            setName("Москва");
        }});
        addressRegistry.setArea(new Area() {{
            setId(672008111L);
            setCode("888");
            setName("Сведения о районе в регионе");
            setType(new Names() {{
                setFull("Тип");
                setShort("Шорт");
            }});
            setCodeOMKTE("0001");
            setCodeBTI("0002");
        }});
        addressRegistry.setAreaOMKTE(new AreaOMKTE() {{
            setId(672008222L);
            setCode("0403");
            setName("Сведения об округе (по ОМК ТЕ)");
            setType(new Names() {{
                setFull("Тип");
                setShort("Шорт");
            }});
        }});
        addressRegistry.setCity(new City() {{
            setId(672008333L);
            setCode("000");
            setName("Город");
            setType(new Names() {{
                setFull("Тип");
                setShort("Шорт");
            }});
            setCodeOMKTM("001");
            setCodeBTI("002");
        }});
        addressRegistry.setPlace(new Place() {{
            setId(672008444L);
            setCode("000");
            setName("Сведения о населенном пункте");
            setType(new Names() {{
                setFull("Населенный пункт");
                setShort("НП");
            }});
            setCodeOMKTM("111");
            setCodeBTI("222");
        }});
        addressRegistry.setPlan(new Plan() {{
            setId(672008555L);
            setCode("0000");
            setName("Сведения о планировочной структуре");
            setType(new Names() {{
                setFull("планировочная структура");
                setShort("ПС");
            }});
            setCodeBTI("195411");
        }});
        addressRegistry.setStreet(new Street() {{
            setId(672008666L);
            setCode("7041");
            setName("Ивантеевская улица");
            setType(new Names() {{
                setFull("Улица");
                setShort("ул");
            }});
            setCodeOMKUM("020930");
            setCodeBTI("194863");
        }});
        addressRegistry.setBuilding(new Building() {{
            setHouse(new House() {{
                setName("99");
            }});
        }});

        AddAreaAddressRequest addAreaAddressRequest = new AddAreaAddressRequest();
        addAreaAddressRequest.setAreaId(175715882L);
        addAreaAddressRequest.getAddresses().add(addressRegistry);

        assertThrows(Fault.class, () -> areaPTv3.addAreaAddress(addAreaAddressRequest));
    }

    @Test
    @Sql(scripts = {"/sql/area_type.sql", "/sql/addAreaAddressTest2522.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressTest2522_1() throws SOAPException, IOException, JAXBException {

        InputStream inputStream = getClass().getClassLoader().getResourceAsStream("xml/addAreaAddress2522.xml");
        SOAPMessage message = MessageFactory.newInstance(SOAPConstants.SOAP_1_2_PROTOCOL).createMessage(null, inputStream);
        Unmarshaller unmarshaller = JAXBContext.newInstance(AddAreaAddressRequest.class).createUnmarshaller();

        AddAreaAddressRequest request = (AddAreaAddressRequest) unmarshaller.unmarshal(message.getSOAPBody().extractContentAsDocument());
        Fault fault = assertThrows(Fault.class, () -> areaPTv3.addAreaAddress(request));

//        Assertions.assertEquals("Адрес город Москва, Дом Нескольких уже обслуживается данным участком.", fault.getMessage());
        Assertions.assertEquals(1, ((ru.mos.emias.system.v1.faults.BusinessFault) fault.getFaultInfo()).getMessages().getMessages().size());
    }

    @Test
    @Sql(scripts = {"/sql/area_type.sql", "/sql/addAreaAddressTest2531.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressTest2531_1() throws SOAPException, IOException, JAXBException {
        AddAreaAddressRequest request2a = addAreaRequest("xml/addAreaAddress2531_1.xml");
        AddAreaAddressResponse response2a = assertDoesNotThrow(() -> areaPTv3.addAreaAddress(request2a));

        Assertions.assertNotNull(response2a.getAreaAddressIds());
        Assertions.assertEquals(1, response2a.getAreaAddressIds().size());

        AddAreaAddressRequest request2b = addAreaRequest("xml/addAreaAddress2531_2.xml");
        AddAreaAddressResponse response2b = assertDoesNotThrow(() -> areaPTv3.addAreaAddress(request2b));

        Assertions.assertNotNull(response2b.getAreaAddressIds());
        Assertions.assertEquals(1, response2b.getAreaAddressIds().size());

        AddAreaAddressRequest requestMain = addAreaRequest("xml/addAreaAddress2531_3.xml");
        Fault fault = assertThrows(Fault.class, () -> areaPTv3.addAreaAddress(requestMain));

        Assertions.assertNotNull(fault.getMessage());
        Assertions.assertEquals(2, ((ru.mos.emias.system.v1.faults.BusinessFault) fault.getFaultInfo()).getMessages().getMessages().size());

        Assertions.assertEquals("E018", ((ru.mos.emias.system.v1.faults.BusinessFault) fault.getFaultInfo()).getMessages().getMessages().get(0).getCode());
        Assertions.assertEquals("Адрес -801 уже обслуживается участком с ИД 114012649. Адрес, включенный в территорию обслуживания участка, не должен входить в территорию обслуживания другого участка с ИД типа участка 20", ((ru.mos.emias.system.v1.faults.BusinessFault) fault.getFaultInfo()).getMessages().getMessages().get(0).getMessage());

        Assertions.assertEquals("E018", ((ru.mos.emias.system.v1.faults.BusinessFault) fault.getFaultInfo()).getMessages().getMessages().get(1).getCode());
        Assertions.assertEquals("Адрес -800 уже обслуживается участком с ИД 175715882. Адрес, включенный в территорию обслуживания участка, не должен входить в территорию обслуживания другого участка с ИД типа участка 20", ((ru.mos.emias.system.v1.faults.BusinessFault) fault.getFaultInfo()).getMessages().getMessages().get(1).getMessage());
    }

    @Test
    @Sql(scripts = {"/sql/area_type.sql", "/sql/addAreaAddressTest2549.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressTest2549() throws SOAPException, IOException, JAXBException {
        AddAreaAddressRequest request2 = addAreaRequest("xml/addAreaAddress2549.xml");
        AddAreaAddressResponse response2 = assertDoesNotThrow(() -> areaPTv3.addAreaAddress(request2));
        assertNotNull(response2.getAreaAddressIds());

        AddAreaAddressRequest request3 = addAreaRequest("xml/addAreaAddress2549_1.xml");
        Fault fault = assertThrows(Fault.class, () -> areaPTv3.addAreaAddress(request3));
        assertEquals("Адрес город Москва, Улица Рандомная уже обслуживается данным участком.", fault.getMessage());
    }

    @Test
    @Sql(scripts = {"/sql/area_type.sql", "/sql/addAreaAddressTest2558.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void addAreaAddressTest2558() throws Throwable {

        AddAreaAddressRequest request = addAreaRequest("xml/addAreaAddress2558.xml");
        AddAreaAddressResponse response = assertDoesNotThrow(() -> areaPTv3.addAreaAddress(request));
        assertNotNull(response.getAreaAddressIds());

        MockEsuService receiveService = (MockEsuService) esuService;
        Assertions.assertFalse(receiveService.getMessages().isEmpty());

        receiveService.getMessages().forEach(esuOutput -> {
            if (esuOutput.getMessage().contains("174712126")) {
                assertTrue(esuOutput.getMessage().contains(">addAreaAddress<"));
            }
        });
    }

    private AddAreaAddressRequest addAreaRequest(String filePath) throws SOAPException, JAXBException, IOException {
        InputStream inputStream = getClass().getClassLoader().getResourceAsStream(filePath);
        SOAPMessage message = MessageFactory.newInstance(SOAPConstants.SOAP_1_2_PROTOCOL).createMessage(null, inputStream);
        Unmarshaller unmarshaller = JAXBContext.newInstance(AddAreaAddressRequest.class).createUnmarshaller();

        return (AddAreaAddressRequest) unmarshaller.unmarshal(message.getSOAPBody().extractContentAsDocument());
    }
}
