package area.service;

import moscow.ptnl.contingent2.rmr.event.JobExecutionInfoMsg;
import org.junit.jupiter.api.Test;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import java.io.File;
import java.io.InputStream;

public class EsuInputMessageTest {

    @Test
    public void jemsgTest() {
        try {
            InputStream inputFile = EsuInputMessageTest.class.getClassLoader().getResourceAsStream("esu/JEMsg.xml");
            JAXBContext jaxbContext = JAXBContext.newInstance(JobExecutionInfoMsg.class);
            Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
            JobExecutionInfoMsg jobExecutionInfoMsg = (JobExecutionInfoMsg) unmarshaller.unmarshal(inputFile);
        } catch (JAXBException e) {
            e.printStackTrace();
        }
    }
}
