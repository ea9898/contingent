package moscow.ptnl.util;

import java.io.StringReader;
import java.io.StringWriter;
import java.util.GregorianCalendar;
import java.util.Objects;
import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import org.xml.sax.SAXException;

/**
 *
 * @author mkachalov
 */
public class XMLUtil {
    
    private XMLUtil(){}
    
    public static String convertObjectToMessage(Object o, Class ... marshalledTypes) {
        String xmlContent = null;
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance(marshalledTypes);
            Marshaller jaxbMarshaller = jaxbContext.createMarshaller();
            jaxbMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
            StringWriter sw = new StringWriter();
            jaxbMarshaller.marshal(o, sw);
            xmlContent = sw.toString();
        } catch (JAXBException e) {            
            throw new RuntimeException("Ошибка конвертации объекта в XML", e);
        }
        return xmlContent;
    }
    
    public static <T> T convertMessageToObject(String message, Class<T> typeClass, String xsdPath) {
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance(typeClass.getPackage().getName(), typeClass.getClassLoader());
            SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            Schema schema = sf.newSchema(Objects.requireNonNull(Thread.currentThread().getContextClassLoader().getResource(xsdPath)));
            Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
            jaxbUnmarshaller.setSchema(schema);
            StringReader reader = new StringReader(message);

            return (T) jaxbUnmarshaller.unmarshal(reader);
        } catch (JAXBException | SAXException ex) {
            throw new RuntimeException("Ошибка конвертации XML в объект", ex);
        }
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
