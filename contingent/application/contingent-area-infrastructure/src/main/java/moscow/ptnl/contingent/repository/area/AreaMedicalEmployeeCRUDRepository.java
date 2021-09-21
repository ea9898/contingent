package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.domain.area.model.area.AreaHistory;
import moscow.ptnl.contingent.repository.PagingAndSortingRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.ColumnResult;
import javax.persistence.ConstructorResult;
import javax.persistence.SqlResultSetMapping;
import javax.persistence.Tuple;
import java.math.BigInteger;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public interface AreaMedicalEmployeeCRUDRepository extends PagingAndSortingRepository<AreaMedicalEmployees, Long> {

    @Query(countQuery = "select " +
            "count(ame.id) from area_medical_employees as ame " +
            "inner join jl_history jlh on cast(jlh.object_id as numeric) = ame.id " +
            "and jlh.object_type in ('AREA_MEDICAL_EMPLOYEES', 'AREA_EMPLOYEE') " +
            "where ame.area_id = :areaId",
            value = "select " +
                    "ame.medical_employee_job_id as jobId, " +
                    "ame.snils as snils, " +
                    "ame.is_replacement as is_replacement, " +
                    "jlh.user_login as login, " +
                    "jlh.change_date as changeDate, " +
                    "(select jhc.new_value from jl_history_columns jhc where jhc.jrn_id = jlh.id and jhc.column_name = 'startDate') as startDate, " +
                    "(select jhc.new_value from jl_history_columns jhc where jhc.jrn_id = jlh.id and jhc.column_name = 'endDate') as endDate, " +
                    "(select jhc.new_value from jl_history_columns jhc where jhc.jrn_id = jlh.id and jhc.column_name = 'isError') as isError " +
                    "from area_medical_employees as ame " +
                    "inner join jl_history jlh on cast(jlh.object_id as numeric) = ame.id " +
                    "       and jlh.object_type in ('AREA_MEDICAL_EMPLOYEES', 'AREA_EMPLOYEE') " +
                    "where ame.area_id = :areaId " +
                    "order by ame.is_replacement, ame.medical_employee_job_id, jlh.change_date ",
            nativeQuery = true)
    Page<AreaHistory.Event> areaHistory(@Param("areaId") Long areaId, Pageable pageable);

}
