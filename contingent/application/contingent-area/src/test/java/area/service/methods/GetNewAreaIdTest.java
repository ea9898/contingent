package service.methods;

import moscow.ptnl.contingent.area.service.AreaServiceInternal;
import moscow.ptnl.contingent.area.service.AreaServiceInternalImpl;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class GetNewAreaIdTest {

    @Mock
    AreaRepository areaRepository;

    @InjectMocks
    private AreaServiceInternal areaServiceInternal = new AreaServiceInternalImpl();

    @BeforeEach
    public void init() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void testTest() {
        Mockito.when(areaRepository.getNextAreaId()).thenReturn(100L);
        Assertions.assertEquals(100L, (long) areaRepository.getNextAreaId());
    }
}
