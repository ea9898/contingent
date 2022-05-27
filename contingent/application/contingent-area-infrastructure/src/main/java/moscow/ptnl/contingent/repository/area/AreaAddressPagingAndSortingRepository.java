package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.model.area.MoMuPair;
import moscow.ptnl.contingent.repository.PagingAndSortingRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface AreaAddressPagingAndSortingRepository extends PagingAndSortingRepository<AreaAddress, Long>, JpaSpecificationExecutor<AreaAddress> {

    @Query("SELECT DISTINCT new moscow.ptnl.contingent.domain.area.model.area.MoMuPair(ar.moId, ar.muId) FROM AreaAddress aad " +
            "JOIN aad.address ad " +
            "JOIN aad.area ar " +
            "WHERE " +
            "  (:regionOMKTECode IS NOT NULL AND ad.regionTeCode = :regionOMKTECode " +
            "    OR :regionOMKTECode IS NULL AND (ad.areaCodeOmkTe = :areaCodeOmkTe OR ad.regionTeCode = :regionTeCode AND ad.aoLevel = :aoLevelRegionTe) " +
            "  ) AND (aad.endDate IS NULL OR aad.endDate > :endDate)" +
            "  AND ar.areaType.code IN (:areaTypeCodes)" +
            "  AND ar.archived = false"
    )
    Page<MoMuPair> findMoMuList(@Param("areaTypeCodes") List<Long> areaTypeCodes,
                                @Param("areaCodeOmkTe") String areaOMKTECode,
                                @Param("regionOMKTECode") String regionOMKTECode,
                                @Param("regionTeCode") String regionTeCode,
                                @Param("aoLevelRegionTe") String aoLevelRegionTe,
                                @Param("endDate") LocalDate endDate,
                                Pageable paging);
}
