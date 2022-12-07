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
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import moscow.ptnl.contingent.domain.area.model.area.MedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.SearchAreaAddress;
import moscow.ptnl.contingent.error.ContingentException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.transaction.annotation.Transactional;
import ru.mos.emias.contingent2.area.v3.AreaPT;
import ru.mos.emias.contingent2.area.v3.Fault;
import ru.mos.emias.contingent2.area.v3.types.AddAreaAddressRequest;
import ru.mos.emias.contingent2.area.v3.types.SearchAreaRequest;
import ru.mos.emias.contingent2.area.v3.types.SearchAreaResponse;
import ru.mos.emias.contingent2.area.v3.Fault;
import ru.mos.emias.contingent2.area.v3.types.AddAreaAddressRequest;
import ru.mos.emias.contingent2.area.v3.types.SearchAreaRequest;

import javax.sql.DataSource;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPConstants;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import java.io.IOException;
import java.io.InputStream;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = {PersistenceConfiguration.class, MockConfiguration.class})
@Transactional
public class SearchAreaTest {

    @Autowired
    private AreaService areaServiceDomain;

    @Autowired
    private AreaPT areaPTv3;

    private static final PageRequest PR = PageRequest.of(0, 10);
    private static final List EL = Collections.emptyList();

    @BeforeAll
    public static void init(@Qualifier("contingentDataSource") DataSource dataSource) throws LiquibaseException, SQLException {
        Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(new JdbcConnection(dataSource.getConnection()));
        Liquibase liquibase = new Liquibase("changelog/area/versions/master.xml", new FileSystemResourceAccessor("../database"), database);
        liquibase.update("");
    }

    @Test
    public void searchAreaExceptionTest() {
        Throwable exception = assertThrows(ContingentException.class, () -> areaServiceDomain.searchArea(null, null, EL,
                EL, null, null, null, null, null, EL, EL, null, null, false));
        assertEquals(exception.getMessage(), "Не заданы критерии поиска");
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchAreaAllParametersTest() {
        Page<AreaInfo> areas = assertDoesNotThrow(() -> areaServiceDomain.searchArea(
                1L, 204L, EL, Collections.singletonList(10L), null, null,
                123, null, false, EL, EL, null, PR, false));
        assertNotNull(areas);
        assertEquals(areas.getNumberOfElements(), 1);
        assertEquals(areas.getNumberOfElements(), areas.getContent().size());
        assertEquals(areas.getContent().get(0).getArea().getId(), (Long) 2L);
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchAreaByMedicalEmployeesTest() {
        Page<AreaInfo> areas = assertDoesNotThrow(() -> areaServiceDomain.searchArea(
                null, null, EL, Collections.singletonList(10L),
                null, null, null, null, false, Arrays.asList(new MedicalEmployee() {{
                                                                 setMedicalEmployeeJobId(123L);
                                                             }},
                        new MedicalEmployee() {{
                            setSnils("snilscode1");
                        }}), EL, null, PR, false));
        assertNotNull(areas);
        assertEquals(areas.getNumberOfElements(), 1);
        assertEquals(areas.getNumberOfElements(), areas.getContent().size());
        assertEquals(areas.getContent().get(0).getArea().getId(), (Long) 2L);
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchAreaByAddressesExactMatchTest() {
        Page<AreaInfo> areas = assertDoesNotThrow(() -> areaServiceDomain.searchArea(
                null, null, EL, Collections.singletonList(10L),
                null, null, null, null, false, EL,
                Arrays.asList(new SearchAreaAddress() {{
                    setGlobalIdNsi(111L);
                    setAreaOMKTEcode("");
                    setRegionOMKTEcode("");
                }}),
                true, PR, false));
        assertNotNull(areas);
        assertEquals(1, areas.getNumberOfElements());
        assertEquals(areas.getNumberOfElements(), areas.getContent().size());
        assertEquals((Long) 2L, areas.getContent().get(0).getArea().getId());
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchAreaByListAddressesEmpty() {
        Page<AreaInfo> areas = assertDoesNotThrow(() -> areaServiceDomain.searchArea(
                null, 204L, Arrays.asList(100L), Collections.singletonList(10L),
                null, null, null, null, false, EL,
                EL,
                true, PR, true));


        assertNotNull(areas);
        assertEquals(2, areas.getContent().size());

        for (AreaInfo aInfo : areas) {
            assertEquals(204L, aInfo.getArea().getMoId().longValue());
            assertEquals(100L, aInfo.getArea().getMuId().longValue());
        }

    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchAreaByListAddressesEmpty2() {
        Page<AreaInfo> areas = assertDoesNotThrow(() -> areaServiceDomain.searchArea(
                null, 136L, Arrays.asList(100L), Collections.singletonList(20L),
                null, null, null, null, false, EL,
                EL,
                true, PR, true));


        assertNotNull(areas);
        assertEquals(2, areas.getContent().size());

        for (AreaInfo aInfo : areas) {
            assertEquals(136L, aInfo.getArea().getMoId().longValue());
            assertEquals(100L, aInfo.getArea().getMuId().longValue());
        }
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql", "/sql/searchArea2492.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchArea2492() {
        SearchAreaAddress searchAreaAddress = new SearchAreaAddress();
        searchAreaAddress.setAoLevel("1");
        searchAreaAddress.setGlobalIdNsi(69597949);
        searchAreaAddress.setRegionOMKTEcode("0100");
        searchAreaAddress.setAreaOMKTEcode("0003");
        searchAreaAddress.setStreetCode("1041");

        Throwable exception = assertThrows(ContingentException.class, () -> areaServiceDomain.searchArea(
                null, null, null, null,
                null, null, null, null, false, EL,
                Collections.singletonList(searchAreaAddress), true, PR, true));
        assertEquals(exception.getMessage(), "Некорректный уровень адреса 1");
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql", "/sql/searchArea2494.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchArea2494() {
//        assertDoesNotThrow(() -> areaServiceDomain.addMoAddress());

        SearchAreaAddress searchAreaAddress = new SearchAreaAddress();
        searchAreaAddress.setAoLevel("8");
        searchAreaAddress.setGlobalIdNsi(68669240);
        searchAreaAddress.setRegionOMKTEcode("0400");
        searchAreaAddress.setAreaOMKTEcode("0410");
        searchAreaAddress.setStreetCode("1731");

        assertDoesNotThrow(() -> areaServiceDomain.searchArea(
                null, null, null, null,
                null, null, null, null, false, EL,
                Collections.singletonList(searchAreaAddress), true, PR, true));
    }


    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql", "/sql/searchArea2494.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchArea2494_2() {
        SearchAreaAddress searchAreaAddress = new SearchAreaAddress();
        searchAreaAddress.setAoLevel("8");
        searchAreaAddress.setGlobalIdNsi(-99999998L);
        searchAreaAddress.setRegionOMKTEcode("0400");
        searchAreaAddress.setAreaOMKTEcode("0403");
        searchAreaAddress.setStreetCode("7041");

        assertDoesNotThrow(() -> areaServiceDomain.searchArea(
                null, null, null, null,
                null, null, null, null, null, EL,
                Collections.singletonList(searchAreaAddress), true, PR, true));
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchAreaTest.sql", "/sql/searchArea2500.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchArea2500() {
        SearchAreaAddress searchAreaAddress = new SearchAreaAddress();
        searchAreaAddress.setAoLevel("7");
        searchAreaAddress.setGlobalIdNsi(-99999007L);
        searchAreaAddress.setAreaOMKTEcode("0403");
        searchAreaAddress.setCityCode("000");
        searchAreaAddress.setPlaceCode("000");
        searchAreaAddress.setPlanCode("0000");
        searchAreaAddress.setStreetCode("7041");

        assertDoesNotThrow(() -> areaServiceDomain.searchArea(
                null, null, null, null,
                null, null, null, null, null, EL,
                Collections.singletonList(searchAreaAddress), false, PR, true));
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchArea2538.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchArea() throws SOAPException, IOException, JAXBException {
        SearchAreaAddress searchAreaAddress = new SearchAreaAddress();
        searchAreaAddress.setAoLevel("7");
        searchAreaAddress.setGlobalIdNsi(-999990077L);
        searchAreaAddress.setAreaOMKTEcode("0403");
        searchAreaAddress.setCityCode("000");
        searchAreaAddress.setPlaceCode("000");
        searchAreaAddress.setPlanCode("0000");
        searchAreaAddress.setStreetCode("7041");

        InputStream inputStream = getClass().getClassLoader().getResourceAsStream("xml/searchArea2538.xml");
        SOAPMessage message = MessageFactory.newInstance(SOAPConstants.SOAP_1_2_PROTOCOL).createMessage(null, inputStream);
        Unmarshaller unmarshaller = JAXBContext.newInstance(SearchAreaRequest.class).createUnmarshaller();

        SearchAreaRequest request = (SearchAreaRequest) unmarshaller.unmarshal(message.getSOAPBody().extractContentAsDocument());
        SearchAreaResponse response = assertDoesNotThrow(() -> areaPTv3.searchArea(request));

        Throwable exception = assertThrows(ContingentException.class, () -> areaServiceDomain.searchArea(
                request.getAreaTypeClassCode(), request.getMoId(), request.getMuIds(), request.getAreaTypeCodes(),
                null, request.getMuIds(), request.getNumber(), request.getDescription(), request.isIsArchived(), EL,
                Collections.singletonList(searchAreaAddress), false, PR, true));

        assertEquals("Адрес с global_id -999990077 не найден в НСИ.2", exception.getMessage());
        Assertions.assertEquals(1, response.getResult().getAreas().size());
    }

    @Test
    @Sql(scripts = {"/sql/areaTypeClass.sql", "/sql/searchArea2538.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchAreaFoundAddressAoLevelEqualsOne() throws SOAPException, IOException, JAXBException {

        InputStream inputStream = getClass().getClassLoader().getResourceAsStream("xml/searchArea2538.xml");
        SOAPMessage message = MessageFactory.newInstance(SOAPConstants.SOAP_1_2_PROTOCOL).createMessage(null, inputStream);
        Unmarshaller unmarshaller = JAXBContext.newInstance(SearchAreaRequest.class).createUnmarshaller();

        SearchAreaRequest request = (SearchAreaRequest) unmarshaller.unmarshal(message.getSOAPBody().extractContentAsDocument());
        SearchAreaResponse response = assertDoesNotThrow(() -> areaPTv3.searchArea(request));

//        Throwable exception = assertThrows(Fault.class, () -> areaPTv3.searchArea(request));
//        Assertions.assertNotNull(exception);
//        Assertions.assertEquals("Адрес с global_id -100 не найден в НСИ.2", exception.getMessage());

        Assertions.assertEquals(1, response.getResult().getAreas().size());
    }

    @Test // ЕСЛИ адрес найден И его уровень aolevel =1, ТО система формирует ошибку С_УУ_107
    @Sql(scripts = {"/sql/area_type.sql", "/sql/searchArea2539.sql"}, executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    public void searchArea2539() throws SOAPException, JAXBException, IOException {
        SearchAreaRequest request = soapToObject("xml/searchArea2539.xml");
        SearchAreaResponse response = assertDoesNotThrow(() -> areaPTv3.searchArea(request));

        SearchAreaAddress searchAreaAddress = new SearchAreaAddress();
        searchAreaAddress.setAoLevel("1");
        searchAreaAddress.setGlobalIdNsi(-1701L);
        searchAreaAddress.setAreaOMKTEcode("0403");
        searchAreaAddress.setCityCode("000");
        searchAreaAddress.setPlaceCode("000");
        searchAreaAddress.setPlanCode("0000");
        searchAreaAddress.setStreetCode("7041");

        Throwable exception = assertThrows(ContingentException.class, () -> areaServiceDomain.searchArea(
                request.getAreaTypeClassCode(), request.getMoId(), request.getMuIds(), request.getAreaTypeCodes(),
                null, request.getMuIds(), request.getNumber(), request.getDescription(), request.isIsArchived(), EL,
                Collections.singletonList(searchAreaAddress), false, PR, true));

        assertEquals("Некорректный уровень адреса 1", exception.getMessage());
        Assertions.assertEquals(1, response.getResult().getAreas().size());
    }

    private SearchAreaRequest soapToObject(String filePath) throws SOAPException, JAXBException, IOException {
        InputStream inputStream = getClass().getClassLoader().getResourceAsStream(filePath);
        SOAPMessage message = MessageFactory.newInstance(SOAPConstants.SOAP_1_2_PROTOCOL).createMessage(null, inputStream);
        Unmarshaller unmarshaller = JAXBContext.newInstance(SearchAreaRequest.class).createUnmarshaller();

        return (SearchAreaRequest) unmarshaller.unmarshal(message.getSOAPBody().extractContentAsDocument());

    }
}
