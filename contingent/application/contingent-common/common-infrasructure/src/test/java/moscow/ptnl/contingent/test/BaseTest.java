package moscow.ptnl.contingent.test;

import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

/**
 *
 * @author m.kachalov
 */
@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = {
    TestContextConfiguration.class
})
@TestPropertySource(locations = "classpath:persistence-test.properties")
public abstract class BaseTest {
    
}
