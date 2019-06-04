package moscow.ptnl.util;

import java.io.StringWriter;
import java.util.GregorianCalendar;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

/**
 *
 * @author mkachalov
 */
public class XMLUtil {
    
    private XMLUtil(){}
    
    public static String convertEventObjectToMessage(Object o, Class ... marshalledTypes) {
        String xmlContent = null;
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance(marshalledTypes);
            Marshaller jaxbMarshaller = jaxbContext.createMarshaller();
            jaxbMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
            StringWriter sw = new StringWriter();
            jaxbMarshaller.marshal(o, sw);
            xmlContent = sw.toString();
        } catch (JAXBException e) {            
            throw new RuntimeException("ошибка конвертации объекта в XML", e);
        }
        return xmlContent;
    }
    
    public static XMLGregorianCalendar getCurrentDate() {
        GregorianCalendar gcal = new GregorianCalendar();
        XMLGregorianCalendar xgcal = null;
        try {
            xgcal = DatatypeFactory.newInstance().newXMLGregorianCalendar(gcal);
        } catch (DatatypeConfigurationException e) {            
            throw new RuntimeException("ошибка получения даты", e);
        }
        return xgcal;
    }
    
}
