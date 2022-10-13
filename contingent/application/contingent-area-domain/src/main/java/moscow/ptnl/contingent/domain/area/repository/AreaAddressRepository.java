package moscow.ptnl.contingent.domain.area.repository;

import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.model.area.MoMuPair;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.NoRepositoryBean;

import java.time.LocalDate;
import java.util.List;

@NoRepositoryBean
public interface AreaAddressRepository {

    List<AreaAddress> getActiveAreaAddresses(long moId, long areaTypeCode);

    List<AreaAddress> getActiveAreaAddressesV3(long moId, AreaType areaTypeCode, Addresses globalIdNsi);

    List<AreaAddress> getActiveAreaAddressesLevel8(long moId, AreaType areaTypeCode, Addresses addresses);

    List<AreaAddress> getActiveAreaAddressesLevel65(long moId, AreaType areaTypeCode, Addresses addresses);

    List<AreaAddress> getActiveAreaAddressesLevel6(long moId, AreaType areaTypeCode, Addresses addresses);

    List<AreaAddress> getActiveAreaAddressesLevel4(long moId, AreaType areaTypeCode, Addresses addresses);

    List<AreaAddress> getActiveAreaAddressesLevel25(long moId, AreaType areaTypeCode, Addresses addresses);

    List<AreaAddress> getActiveAreaAddressesLevel2(long moId, AreaType areaTypeCode, Addresses addresses);

    List<AreaAddress> getActiveAreaAddressesLevel7(long moId, AreaType areaTypeCode, Addresses addresses);

    List<AreaAddress> findAreaAddresses(List<Long> moAddressIds);

    List<AreaAddress> findActualAreaAddresses(List<Long> moAddressIds);

    List<AreaAddress> findAreaAddressesActual(List<Long> moAddressIds);

    Page<AreaAddress> findAreaAddressesByAreaId(Long moId, List<Long> areaIds, Pageable paging);

    List<AreaAddress> findAreaAddressByAddressIds(List<Long> addressIds);

    void delete(AreaAddress areaAddress);

    List<AreaAddress> saveAll(List<AreaAddress> addresses);

    AreaAddress save(AreaAddress areaAddress);

    Page<MoMuPair> findMoMuList(List<Long> areaTypeCodes, String areaOMKTECode, String regionOMKTECode,
                                String regionTeCode, String aoLevelRegionTe, LocalDate endDate, PageRequest paging);

    Page<MoMuPair> findMoMuList(List<Long> areaTypeCodes, List<Long> addressIds, LocalDate endDate, PageRequest paging);
}
