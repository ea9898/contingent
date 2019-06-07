package moscow.ptnl.contingent.util;

import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import java.io.StringWriter;
import java.lang.invoke.MethodHandles;

public class Utils {

    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    public static String convertEventObjectToMessage(Object o, Class<?> clazz) {
        String xmlContent = null;

        try {
            JAXBContext jaxbContext;
            jaxbContext = JAXBContext.newInstance(clazz);
            Marshaller jaxbMarshaller = jaxbContext.createMarshaller();
            jaxbMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
            StringWriter sw = new StringWriter();
            jaxbMarshaller.marshal(o, sw);
            xmlContent = sw.toString();
        } catch (JAXBException e) {
            LOG.error("Ошибка конвертации объекта в XML", e);
        }
        return xmlContent;
    }


}
