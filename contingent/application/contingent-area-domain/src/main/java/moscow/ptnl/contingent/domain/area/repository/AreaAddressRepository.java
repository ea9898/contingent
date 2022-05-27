package moscow.ptnl.contingent.domain.area.repository;

import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.model.area.MoMuPair;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.NoRepositoryBean;

import java.time.LocalDate;
import java.util.List;

@NoRepositoryBean
public interface AreaAddressRepository {

    List<AreaAddress> getActiveAreaAddresses(long moId, long areaTypeCode);

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
}
