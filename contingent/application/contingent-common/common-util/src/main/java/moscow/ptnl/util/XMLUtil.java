package moscow.ptnl.util;

import java.io.StringReader;
import java.io.StringWriter;
import java.util.AbstractList;
import java.util.Collections;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Objects;
import java.util.RandomAccess;
import javax.xml.XMLConstants;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.Unmarshaller;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
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
            Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
            if (xsdPath != null) {
                SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
                Schema schema = sf.newSchema(Objects.requireNonNull(Thread.currentThread().getContextClassLoader().getResource(xsdPath)));            
                jaxbUnmarshaller.setSchema(schema);
            }
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

    public static List<Node> asList(NodeList n) {
        return n.getLength() == 0 ? Collections.emptyList(): new NodeListWrapper(n);
    }

    static final class NodeListWrapper extends AbstractList<Node> implements RandomAccess {

        private final NodeList list;

        NodeListWrapper(NodeList l) {
            list=l;
        }

        @Override
        public Node get(int index) {
            return list.item(index);
        }

        @Override
        public int size() {
            return list.getLength();
        }
    }
}
