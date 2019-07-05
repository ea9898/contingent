package moscow.ptnl.contingent.esuInputTasks;

import javax.xml.datatype.XMLGregorianCalendar;
import java.time.LocalDate;

class EsuInputTaskHelper {

    static LocalDate convertToLocalDate(XMLGregorianCalendar calendar) {
        return calendar == null ? null : LocalDate.of(calendar.getYear(), calendar.getMonth(), calendar.getDay());
    }
}
