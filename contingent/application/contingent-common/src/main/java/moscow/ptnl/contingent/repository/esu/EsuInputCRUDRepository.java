package moscow.ptnl.contingent.repository.esu;

import java.time.LocalDateTime;
import java.util.List;
import moscow.ptnl.contingent.domain.esu.EsuInput;
import moscow.ptnl.contingent.domain.esu.EsuStatusType;
import moscow.ptnl.contingent.repository.PagingAndSortingRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface EsuInputCRUDRepository extends PagingAndSortingRepository<EsuInput, Long> {
    
    @Query("select o.id from EsuInput o where o.receivedTime < :receivedTime and o.status = :status order by o.id")
    Page<Long> findByReceivedTimeBeforeAndStatus(@Param("receivedTime") LocalDateTime receivedTime, @Param("status")  EsuStatusType status, Pageable pageable);
    
    @Query("select o.id from EsuInput o where o.receivedTime < :receivedTime order by o.id")
    Page<Long> findByReceivedTimeBefore(@Param("receivedTime") LocalDateTime receivedTime, Pageable pageable);
    
    @Query("delete from EsuInput o where o.id in (:idList)")
    long deleteByIds(@Param("idList") List<Long> idList);
    
}
