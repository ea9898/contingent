package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaMuService;
import moscow.ptnl.contingent.repository.PagingAndSortingRepository;

import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface AreaMuServiceCRUDRepository extends PagingAndSortingRepository<AreaMuService, Long> {

    @Query("UPDATE AreaMuService ms SET ms.endDate = :newEndDate WHERE ms.muId = :servicedMuId" +
            " AND ms.area = :area AND (ms.endDate IS NULL OR ms.endDate >= :searchEndDate)")
    @Modifying
    void closeAreaMuServices(@Param("servicedMuId") Long servicedMuId, @Param("area") Area area,
                             @Param("searchEndDate") LocalDate searchEndDate, @Param("newEndDate") LocalDate newEndDate);
}
