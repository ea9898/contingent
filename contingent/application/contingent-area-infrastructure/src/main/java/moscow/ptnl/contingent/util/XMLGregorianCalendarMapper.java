package moscow.ptnl.contingent.util;

import org.springframework.stereotype.Component;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.GregorianCalendar;

@Component
public class XMLGregorianCalendarMapper {

    public static XMLGregorianCalendar entityToDtoTransform(LocalDateTime entityObject) {
        GregorianCalendar gcal = GregorianCalendar.from(entityObject.atZone(ZoneId.systemDefault()));

        try {
            return DatatypeFactory.newInstance().newXMLGregorianCalendar(gcal);
        }
        catch (DatatypeConfigurationException e) {
            return null;
        }
    }

    public static LocalDateTime dtoToEntityTransform(XMLGregorianCalendar dtoObject) {
        return null;
    }

    public static XMLGregorianCalendar getNow() {
        return entityToDtoTransform(LocalDateTime.now());
    }
}
