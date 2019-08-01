package moscow.ptnl.contingent.area.service;


import moscow.ptnl.contingent.area.error.Validation;
import org.aspectj.lang.annotation.Before;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class AreaServiceHelperTest {

    private AreaServiceHelper areaServiceHelper;

    @BeforeEach
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
