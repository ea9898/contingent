package moscow.ptnl.contingent.repository.history;

import moscow.ptnl.contingent.domain.area.model.area.AreaFullHistory;
import moscow.ptnl.contingent.domain.history.HistoryEvent;
import moscow.ptnl.contingent.repository.CommonRepository;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 *
 * @author sorlov
 */
@Repository
@Transactional(propagation = Propagation.MANDATORY)
public interface HistoryEventCRUDRepository extends CommonRepository<HistoryEvent, Long> {

    @Query(value = "select " +
            "ame.medical_employee_job_id as medicalEmployeeJobId, " +
            "jlh.user_login as userLogin, " +
            "jlh.job_info_id as userJobId, " +
            "jlh.change_date as updateDate, " +
            "jhc1.old_value as startDateOld, jhc1.new_value as startDateNew, " +
            "jhc2.old_value as endDateOld, jhc2.new_value as endDateNew, " +
            "jhc3.old_value as isErrorOld, jhc3.new_value as isErrorNew, " +
            "jhc4.old_value as isReplacementOld, jhc4.new_value as isReplacementNew, " +
            "jhc5.old_value as tempDutyStartDateOld, jhc5.new_value as tempDutyStartDateNew " +
            "from area_medical_employees as ame " +
            "inner join jl_history jlh on cast(jlh.object_id as numeric) = ame.id " +
            " and jlh.object_type in ('AREA_MEDICAL_EMPLOYEES', 'AREA_EMPLOYEE') " +
            "left join jl_history_columns jhc1 on jhc1.jrn_id = jlh.id and jhc1.column_name = 'startDate' " +
            "left join jl_history_columns jhc2 on jhc2.jrn_id = jlh.id and jhc2.column_name = 'endDate' " +
            "left join jl_history_columns jhc3 on jhc3.jrn_id = jlh.id and jhc3.column_name = 'isError' " +
            "left join jl_history_columns jhc4 on jhc4.jrn_id = jlh.id and jhc4.column_name = 'isReplacement' " +
            "left join jl_history_columns jhc5 on jhc5.jrn_id = jlh.id and jhc5.column_name = 'tempDutyStartDate' " +
            "where ame.area_id = :areaId " +
            "order by jlh.id ",
            nativeQuery = true)
    List<AreaFullHistory.MedicalEmployeeEvent> findMedicalEmployeeEvents(@Param("areaId") Long areaId);

    @Query(value = "select " +
            "jlh.user_login as userLogin, " +
            "jlh.job_info_id as userJobId, " +
            "jlh.change_date as updateDate, " +
            "jhc1.old_value as descriptionOld, jhc1.new_value as descriptionNew, " +
            "jhc2.old_value as numberOld, jhc2.new_value as numberNew, " +
            "jhc3.old_value as createDateOld, jhc3.new_value as createDateNew, " +
            "jhc4.old_value as archivedOld, jhc4.new_value as archivedNew " +
            "from jl_history as jlh " +
            "left join jl_history_columns jhc1 on jhc1.jrn_id = jlh.id and jhc1.column_name = 'description' " +
            "left join jl_history_columns jhc2 on jhc2.jrn_id = jlh.id and jhc2.column_name = 'number' " +
            "left join jl_history_columns jhc3 on jhc3.jrn_id = jlh.id and jhc3.column_name = 'createDate' " +
            "left join jl_history_columns jhc4 on jhc4.jrn_id = jlh.id and jhc4.column_name = 'archived' " +
            "where cast(jlh.object_id as numeric) = :areaId " +
            " and jlh.object_type in ('AREA') " +
            "order by jlh.id ",
            nativeQuery = true)
    List<AreaFullHistory.AreaEvent> findAreaEvents(@Param("areaId") Long areaId);
}
