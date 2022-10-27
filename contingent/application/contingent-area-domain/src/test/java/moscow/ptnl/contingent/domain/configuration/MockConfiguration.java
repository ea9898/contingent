package moscow.ptnl.contingent.domain.configuration;

import moscow.ptnl.contingent.domain.area.Algorithms;
import moscow.ptnl.contingent.domain.area.AlgorithmsHelper;
import moscow.ptnl.contingent.domain.area.MappingDomainService;
import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import org.mockito.Mockito;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MockConfiguration {

    @MockBean
    public AlgorithmsHelper algorithmsHelper;

    @Bean
    public MappingDomainService mappingDomain(){
        return new MappingDomainService() {
            @Override
            public Addresses dtoToEntityTransform(AddressRegistry addressRegistry) {
                return null;
            }
        };
    }

    @Bean
    public Algorithms algorithms(){
        return new Algorithms(Mockito.mock(AlgorithmsHelper.class));
    }
}
