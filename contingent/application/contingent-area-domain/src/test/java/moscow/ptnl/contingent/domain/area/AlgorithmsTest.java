package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import moscow.ptnl.contingent.domain.area.repository.MoAddressRepository;
import moscow.ptnl.contingent.domain.configuration.MockConfiguration;
import moscow.ptnl.contingent.domain.configuration.MockRepositoriesConfiguration;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.util.Assert;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doReturn;

@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes= {MockRepositoriesConfiguration.class, MockConfiguration.class})
class AlgorithmsTest{

    @Autowired
    private Algorithms algorithms;

    @Autowired
    private MoAddressRepository moAddressRepository;

    @Autowired
    private SettingService settingService;

    @Test
    void searchServiceDistrictMOByAddressNoMoAddresses() {

        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(99999L);
        addressRegistry.setAoLevel("8");

        AreaType areaType = new AreaType();
        areaType.setCode(10L);

        Addresses addresses = new Addresses();

        MoAddress moAddress = new MoAddress();
        moAddress.setAddress(addresses);

        doReturn(new ArrayList<>()).when(moAddressRepository).getActiveMoAddresses(areaType);

        Validation validation = new Validation();

        algorithms.searchServiceDistrictMOByAddress(areaType, addressRegistry, validation);
    }

    @Test
    void searchServiceDistrictMOByAddressLevel8AndExact() {

        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(99999L);
        addressRegistry.setAoLevel("8");

        AreaType areaType = new AreaType();
        areaType.setCode(10L);

        Addresses addresses = new Addresses();
        addresses.setGlobalId(99999L);
        addresses.setAoLevel("8");

        MoAddress moAddress = new MoAddress();
        moAddress.setAddress(addresses);

        doReturn(Collections.singletonList(moAddress)).when(moAddressRepository).getActiveMoAddressByGlobalId(99999L, areaType);

        doReturn(Arrays.asList(10L, 20L)).when(settingService).par31();
        doReturn(Boolean.TRUE).when(settingService).getSettingProperty(SettingService.PAR_32);

        Validation validation = new Validation();

        List<MoAddress> moAddressOut = algorithms.searchServiceDistrictMOByAddress(areaType, addressRegistry, validation);
        assertFalse(moAddressOut.isEmpty());
    }

    @Test
    void searchServiceDistrictMOByAddressLevel() {

        AddressRegistry addressRegistry = new AddressRegistry();
        addressRegistry.setGlobalIdNsi(99999L);
        addressRegistry.setAoLevel("8");

        AreaType areaType = new AreaType();
        areaType.setCode(10L);

        Addresses addresses = new Addresses();
        addresses.setGlobalId(99999L);
        addresses.setAoLevel("8");

        MoAddress moAddress = new MoAddress();
        moAddress.setAddress(addresses);

        doReturn(Collections.singletonList(moAddress)).when(moAddressRepository).getActiveMoAddressByGlobalIdAndLevel(99999L, "8", areaType);

        doReturn(Arrays.asList(10L, 20L)).when(settingService).getSettingProperty(SettingService.PAR_31);
        doReturn(Boolean.FALSE).when(settingService).getSettingProperty(SettingService.PAR_32);

        Validation validation = new Validation();

        List<MoAddress> moAddressOut = algorithms.searchServiceDistrictMOByAddress(areaType, addressRegistry, validation);
        assertFalse(moAddressOut.isEmpty());
    }
}
