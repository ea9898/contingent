package moscow.ptnl.contingent.domain.test;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.HashSet;
import java.util.Set;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import org.junit.jupiter.api.Test;

/**
 *
 * @author m.kachalov
 */
public class AreaTest {
    
    @Test
    public void testSetMedicalEmployees(){
        Area area = new Area();
        
        Set<AreaMedicalEmployees> medicalEmployees = new HashSet<>();
        
        AreaMedicalEmployees emp1 = new AreaMedicalEmployees();
        emp1.setId(1L);
        emp1.setReplacement(Boolean.FALSE);
        medicalEmployees.add(emp1);
        
        AreaMedicalEmployees emp2 = new AreaMedicalEmployees();
        emp2.setId(2L);
        emp2.setReplacement(Boolean.FALSE);
        emp2.setEndDate(LocalDate.now(ZoneId.systemDefault()).minusYears(1));
        medicalEmployees.add(emp2);
        
        AreaMedicalEmployees emp3 = new AreaMedicalEmployees();
        emp3.setId(3L);
        emp3.setReplacement(Boolean.TRUE);
        medicalEmployees.add(emp3);
        
        area.setMedicalEmployees(medicalEmployees);
        
        assertNotNull(area.getMedicalEmployees());
        assertEquals(3, area.getMedicalEmployees().size());
        assertEquals(2, area.getActualMedicalEmployees().size());
        assertEquals(1, area.getActualMainMedicalEmployees().size());
        assertEquals(1, area.getActualReplacementMedicalEmployees().size());
    }
    
}
