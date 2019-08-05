package service.methods;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import service.BaseTest;

public class GetNewAreaIdTest extends BaseTest {

    @Test
    public void testGetNewAreaId() {
        Mockito.when(areaRepository.getNextAreaId()).thenReturn(100L);
        Assertions.assertEquals(100L, (long) areaRepository.getNextAreaId());
    }
}
