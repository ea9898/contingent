package moscow.ptnl.contingent.area.transform.model;

import moscow.ptnl.contingent.area.transform.Transform;
import org.springframework.stereotype.Component;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.GregorianCalendar;

@Component
public class XMLGregorianCalendarMapper implements Transform<XMLGregorianCalendar, LocalDateTime> {

    @Override
    public XMLGregorianCalendar entityToDtoTransform(LocalDateTime entityObject) {
        GregorianCalendar gcal = GregorianCalendar.from(entityObject.atZone(ZoneId.systemDefault()));

        try {
            return DatatypeFactory.newInstance().newXMLGregorianCalendar(gcal);
        }
        catch (DatatypeConfigurationException e) {
            return null;
        }
    }

    @Override
    public LocalDateTime dtoToEntityTransform(XMLGregorianCalendar dtoObject) {
        return null;
    }
}
