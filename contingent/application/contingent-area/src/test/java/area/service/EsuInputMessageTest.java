package area.service;

import moscow.ptnl.contingent2.rmr.event.JobExecutionInfoMsg;
import org.junit.jupiter.api.Test;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import java.io.InputStream;

public class EsuInputMessageTest {

    @Test
    public void jemsgTest() {
        try {
            InputStream inputFile = EsuInputMessageTest.class.getClassLoader().getResourceAsStream("esu/jeMsg/JEMsg.xml");
            JAXBContext jaxbContext = JAXBContext.newInstance(JobExecutionInfoMsg.class);
            Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
            JobExecutionInfoMsg jobExecutionInfoMsg = (JobExecutionInfoMsg) unmarshaller.unmarshal(inputFile);
        } catch (JAXBException e) {
            e.printStackTrace();
        }
    }

    @Test
    public void jemsgTest1() {
        try {
            InputStream inputFile = EsuInputMessageTest.class.getClassLoader().getResourceAsStream("esu/jeMsg/JEMsg1.xml");
            JAXBContext jaxbContext = JAXBContext.newInstance(JobExecutionInfoMsg.class);
            Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
            JobExecutionInfoMsg jobExecutionInfoMsg = (JobExecutionInfoMsg) unmarshaller.unmarshal(inputFile);
        } catch (JAXBException e) {
            e.printStackTrace();
        }
    }

    @Test
    public void jemsgTest2() {
        try {
            InputStream inputFile = EsuInputMessageTest.class.getClassLoader().getResourceAsStream("esu/jeMsg/JEMsg2.xml");
            JAXBContext jaxbContext = JAXBContext.newInstance(JobExecutionInfoMsg.class);
            Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
            JobExecutionInfoMsg jobExecutionInfoMsg = (JobExecutionInfoMsg) unmarshaller.unmarshal(inputFile);
        } catch (JAXBException e) {
            e.printStackTrace();
        }
    }
}
