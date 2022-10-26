package moscow.ptnl.contingent.domain.configuration;

import moscow.ptnl.contingent.domain.area.Algorithms;
import moscow.ptnl.contingent.domain.area.AlgorithmsHelper;
import moscow.ptnl.contingent.domain.area.MappingDomainService;
import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import moscow.ptnl.contingent.domain.area.repository.MoAddressRepository;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.mockito.Mockito;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

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

    @Bean
    public SettingService settingService(){
        return new SettingService() {
            @Override
            public <T> T getSettingProperty(String propertyName) {
                return null;
            }

            @Override
            public <T> T getSettingProperty(String propertyName, boolean refesh) {
                return null;
            }

            @Override
            public Long getPar1() {
                return null;
            }

            @Override
            public Long getPar2() {
                return null;
            }

            @Override
            public Long getPar3() {
                return null;
            }

            @Override
            public List<Long> getPar20() {
                return null;
            }

            @Override
            public Boolean getPar4() {
                return null;
            }

            @Override
            public Integer getPar5() {
                return null;
            }

            @Override
            public Integer getPar6() {
                return null;
            }

            @Override
            public List<Long> par31() {
                return null;
            }

            @Override
            public List<Long> par39() {
                return null;
            }

            @Override
            public List<Long> par40() {
                return null;
            }

            @Override
            public Integer getPar42() {
                return null;
            }

            @Override
            public List<Long> getPar43() {
                return null;
            }

            @Override
            public List<Long> getPar44() {
                return null;
            }

            @Override
            public List<Long> getPar45() {
                return null;
            }
        };
    }

    @Bean
    public MoAddressRepository moAddressRepository(){
        return new MoAddressRepository() {
            @Override
            public Page<MoAddress> getActiveMoAddresses(long moId, List<Long> areaTypeCodes, Pageable paging) {
                return null;
            }

            @Override
            public List<MoAddress> getActiveMoAddresses(AreaType areaType) {
                return null;
            }

            @Override
            public List<MoAddress> getActiveMoAddressByGlobalId(Long globalId, AreaType areaType) {
                return null;
            }

            @Override
            public List<MoAddress> getActiveMoAddressByGlobalIdV3(AreaType areaType, Long moId, Addresses addresses) {
                return null;
            }

            @Override
            public List<MoAddress> getActiveMoAddressLevel8(AreaType areaType, Long moId, Addresses addresses) {
                return null;
            }

            @Override
            public List<MoAddress> getActiveMoAddressLevel7(AreaType areaType, Long moId, Addresses addresses) {
                return null;
            }

            @Override
            public List<MoAddress> getActiveMoAddressLevel65(AreaType areaType, Long moId, Addresses addresses) {
                return null;
            }

            @Override
            public List<MoAddress> getActiveMoAddressLevel6(AreaType areaType, Long moId, Addresses addresses) {
                return null;
            }

            @Override
            public List<MoAddress> getActiveMoAddressLevel4(AreaType areaType, Long moId, Addresses addresses) {
                return null;
            }

            @Override
            public List<MoAddress> getActiveMoAddressLevel25(AreaType areaType, Long moId, Addresses addresses) {
                return null;
            }

            @Override
            public Page<MoAddress> getActiveMoAddressesByGlobalIds(List<Long> globalIds) {
                return null;
            }

            @Override
            public Page<MoAddress> getActiveMoAddressesByGlobalIds(List<Long> globalIds, Pageable paging) {
                return null;
            }

            @Override
            public List<MoAddress> getActiveMoAddressByGlobalIdAndLevel(Long globalId, String aoLevel, AreaType areaType) {
                return null;
            }

            @Override
            public Optional<MoAddress> findById(Long id) {
                return Optional.empty();
            }

            @Override
            public void delete(MoAddress moAddress) {

            }

            @Override
            public MoAddress save(MoAddress moAddress) {
                return null;
            }

            @Override
            public List<MoAddress> saveAll(List<MoAddress> moAddress) {
                return null;
            }

            @Override
            public Page<MoAddress> find(long moId, List<Long> addressGlobalIds, List<Long> areaTypeCodes, LocalDate orderDate, String orderName, String orderNumber, String orderOuz, LocalDate orderCreateDate, PageRequest paging) {
                return null;
            }
        };
    }

}
