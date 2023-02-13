package moscow.ptnl.contingent.area.ws;

import java.io.StringReader;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBIntrospector;
import jakarta.xml.bind.Unmarshaller;
import javax.xml.transform.stream.StreamSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author m.kachalov
 */
public class BaseService {
    
    private static final Logger LOG = LoggerFactory.getLogger(BaseService.class);
    
    /**
     * Десериализация XML-строки в Java-объект (JAXB).
     * 
     * @param <T> 
     * @param type корневой класс 
     * @param xmlData строка в формате XML
     * @return
     * @throws Exception 
     */
    public static <T> T jaxbUnmarshall(Class<T> type, String xmlData) throws Exception {
        try {
            JAXBContext context = JAXBContext.newInstance(type.getPackage().getName());
            Unmarshaller unmarshaller = context.createUnmarshaller();
            return (T) JAXBIntrospector.getValue(unmarshaller.unmarshal(new StreamSource(new StringReader(xmlData))));
        } catch (Exception e) {
            LOG.error("unmarshalling error", e);
            throw new Exception(e);
        }
    }
    
}
