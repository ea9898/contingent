package moscow.ptnl.contingent.domain.configuration;

import moscow.ptnl.contingent.domain.area.repository.AddressesRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaAddressRepository;
import moscow.ptnl.contingent.domain.area.repository.MoAddressRepository;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.sysop.repository.SysopRepository;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Configuration;

import javax.persistence.EntityManager;

/**
 *
 * @author sorlov
 */
@Configuration
public class MockRepositoriesConfiguration {

    @MockBean
    public MoAddressRepository moAddressRepository;

    @MockBean
    public AreaAddressRepository areaAddressRepository;

    @MockBean
    public AddressesRepository addressesRepository;

    @MockBean
    public SysopRepository sysopRepository;

    @MockBean
    public SettingService settingService;

    @MockBean
    private EntityManager entityManager;
}
