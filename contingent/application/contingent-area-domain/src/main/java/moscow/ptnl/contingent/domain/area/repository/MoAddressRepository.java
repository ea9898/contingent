package moscow.ptnl.contingent.domain.area.repository;

import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.NoRepositoryBean;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@NoRepositoryBean
public interface MoAddressRepository {

	Page<MoAddress> getActiveMoAddresses(long moId, List<Long> areaTypeCodes, Pageable paging);

	List<MoAddress> getActiveMoAddresses(AreaType areaType);

    List<MoAddress> getActiveMoAddressByGlobalId(Long globalId, AreaType areaType);

    Page<MoAddress> getActiveMoAddressesByGlobalIds(List<Long> globalIds);

    Page<MoAddress> getActiveMoAddressesByGlobalIds(List<Long> globalIds, Pageable paging);

    List<MoAddress> getActiveMoAddressByGlobalIdAndLevel(Long globalId, String aoLevel, AreaType areaType);

    Optional<MoAddress> findById(Long id);

	void delete(MoAddress moAddress);

	MoAddress save(MoAddress moAddress);

	List<MoAddress> saveAll(List<MoAddress> moAddress);

    Page<MoAddress> find(long moId, List<Long> addressGlobalIds, List<Long> areaTypeCodes, LocalDate orderDate, String orderName,
              String orderNumber, String orderOuz, LocalDate orderCreateDate, PageRequest paging);
}
