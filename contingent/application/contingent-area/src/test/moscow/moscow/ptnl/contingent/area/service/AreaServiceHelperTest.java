package moscow.ptnl.contingent.area.service;


import moscow.ptnl.contingent.area.error.Validation;
import org.junit.Before;
import org.junit.Test;

public class AreaServiceHelperTest {

    private AreaServiceHelper areaServiceHelper;

    @Before
    public void setup() {
         areaServiceHelper = new AreaServiceHelper();
    }

    @Test
    public void checkAgeSetupRange() {
        Validation validation = new Validation();
        areaServiceHelper.checkAgeSetupRange(0, 5, 1, 2, "ageMin", "ageMax", validation);

        if (!validation.isSuccess()) {
            System.out.println("Error");
        }
    }
}
