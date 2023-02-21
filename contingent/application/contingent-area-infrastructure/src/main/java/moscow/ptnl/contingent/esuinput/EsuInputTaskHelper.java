package moscow.ptnl.contingent.esuinput;

import javax.xml.datatype.XMLGregorianCalendar;
import java.time.LocalDate;

public class EsuInputTaskHelper {

    public static LocalDate convertToLocalDate(XMLGregorianCalendar calendar) {
        return calendar == null ? null : LocalDate.of(calendar.getYear(), calendar.getMonth(), calendar.getDay());
    }
}
