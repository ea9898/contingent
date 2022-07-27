package moscow.ptnl.contingent.repository.history;

import moscow.ptnl.contingent.domain.area.model.area.AreaOrEmployeeEvent;
import moscow.ptnl.contingent.domain.history.HistoryEvent;
import moscow.ptnl.contingent.repository.CommonRepository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 * @author sorlov
 */
@Repository
@Transactional(propagation = Propagation.MANDATORY)
public interface HistoryEventCRUDRepository extends CommonRepository<HistoryEvent, Long> {

    @Query(countQuery =
            "WITH history AS (\n" +
            "    SELECT\n" +
            "        id,\n" +
            "        object_id,\n" +
            "        (\n" +
            "            CASE\n" +
            "                WHEN object_type = 'AREA_MEDICAL_EMPLOYEES' THEN 1\n" +
            "                WHEN object_type = 'AREA_EMPLOYEE'          THEN 1\n" +
            "                WHEN object_type = 'AREA'                   THEN 2\n" +
            "                ELSE 0\n" +
            "            END\n" +
            "        ) obj_type\n" +
            "    FROM\n" +
            "        jl_history\n" +
            ") SELECT COUNT(jlh.id)\n" +
            "FROM\n" +
            "    history jlh\n" +
            "    LEFT JOIN area_medical_employees ame ON\n" +
            "        CAST(jlh.object_id AS NUMERIC) = ame.id\n" +
            "    AND\n" +
            "        jlh.obj_type = 1\n" +
            "WHERE\n" +
            "        CAST(jlh.object_id AS NUMERIC) IN (\n" +
            "            SELECT\n" +
            "                id\n" +
            "            FROM\n" +
            "                area_medical_employees ame\n" +
            "            WHERE\n" +
            "                ame.area_id = :areaId\n" +
            "        )\n" +
            "    AND\n" +
            "        jlh.obj_type = 1\n" +
            "    OR\n" +
            "        CAST(jlh.object_id AS NUMERIC) = :areaId\n" +
            "    AND\n" +
            "        jlh.obj_type = 2",
            value =
            "WITH history AS (\n" +
            "    SELECT\n" +
            "        id,\n" +
            "        object_id,\n" +
            "        change_date,\n" +
            "        user_login,\n" +
            "        job_info_id,\n" +
            "        (\n" +
            "            CASE\n" +
            "                WHEN object_type = 'AREA_MEDICAL_EMPLOYEES' THEN 1\n" +
            "                WHEN object_type = 'AREA_EMPLOYEE'          THEN 1\n" +
            "                WHEN object_type = 'AREA'                   THEN 2\n" +
            "                ELSE 0\n" +
            "            END\n" +
            "        ) obj_type\n" +
            "    FROM\n" +
            "        jl_history\n" +
            ") SELECT\n" +
            "    ame.medical_employee_job_id AS medicalemployeejobid,\n" +
            "    jlh.obj_type AS objType,\n" +
            "    CAST(jlh.object_id AS NUMERIC) AS objectId,\n" +
            "    jlh.user_login AS userlogin,\n" +
            "    jlh.job_info_id AS userjobid,\n" +
            "    jlh.change_date AS updatedate,\n" +
            "    jhc1.old_value AS startdateold,\n" +
            "    jhc1.new_value AS startdatenew,\n" +
            "    jhc2.old_value AS enddateold,\n" +
            "    jhc2.new_value AS enddatenew,\n" +
            "    jhc3.old_value AS iserrorold,\n" +
            "    jhc3.new_value AS iserrornew,\n" +
            "    jhc4.old_value AS replacementold,\n" +
            "    jhc4.new_value AS replacementnew,\n" +
            "    jhc5.old_value AS tempdutystartdateold,\n" +
            "    jhc5.new_value AS tempdutystartdatenew,\n" +
            "    jhc11.old_value AS descriptionold,\n" +
            "    jhc11.new_value AS descriptionnew,\n" +
            "    jhc12.old_value AS numberold,\n" +
            "    jhc12.new_value AS numbernew,\n" +
            "    jhc13.old_value AS createdateold,\n" +
            "    jhc13.new_value AS createdatenew,\n" +
            "    jhc14.old_value AS archivedold,\n" +
            "    jhc14.new_value AS archivednew\n" +
            "FROM\n" +
            "    history jlh\n" +
            "    LEFT JOIN area_medical_employees ame ON\n" +
            "        CAST(jlh.object_id AS NUMERIC) = ame.id\n" +
            "    AND\n" +
            "        jlh.obj_type = 1\n" +
            "    LEFT JOIN jl_history_columns jhc1 ON\n" +
            "        jhc1.jrn_id = jlh.id\n" +
            "    AND\n" +
            "        jhc1.column_name = 'startDate'\n" +
            "    AND\n" +
            "        jlh.obj_type = 1\n" +
            "    LEFT JOIN jl_history_columns jhc2 ON\n" +
            "        jhc2.jrn_id = jlh.id\n" +
            "    AND\n" +
            "        jhc2.column_name = 'endDate'\n" +
            "    AND\n" +
            "        jlh.obj_type = 1\n" +
            "    LEFT JOIN jl_history_columns jhc3 ON\n" +
            "        jhc3.jrn_id = jlh.id\n" +
            "    AND\n" +
            "        jhc3.column_name = 'isError'\n" +
            "    AND\n" +
            "        jlh.obj_type = 1\n" +
            "    LEFT JOIN jl_history_columns jhc4 ON\n" +
            "        jhc4.jrn_id = jlh.id\n" +
            "    AND\n" +
            "        jhc4.column_name = 'replacement'\n" +
            "    AND\n" +
            "        jlh.obj_type = 1\n" +
            "    LEFT JOIN jl_history_columns jhc5 ON\n" +
            "        jhc5.jrn_id = jlh.id\n" +
            "    AND\n" +
            "        jhc5.column_name = 'tempDutyStartDate'\n" +
            "    AND\n" +
            "        jlh.obj_type = 1\n" +
            "    LEFT JOIN jl_history_columns jhc11 ON\n" +
            "        jhc11.jrn_id = jlh.id\n" +
            "    AND\n" +
            "        jhc11.column_name = 'description'\n" +
            "    AND\n" +
            "        jlh.obj_type = 2\n" +
            "    LEFT JOIN jl_history_columns jhc12 ON\n" +
            "        jhc12.jrn_id = jlh.id\n" +
            "    AND\n" +
            "        jhc12.column_name = 'number'\n" +
            "    AND\n" +
            "        jlh.obj_type = 2\n" +
            "    LEFT JOIN jl_history_columns jhc13 ON\n" +
            "        jhc13.jrn_id = jlh.id\n" +
            "    AND\n" +
            "        jhc13.column_name = 'createDate'\n" +
            "    AND\n" +
            "        jlh.obj_type = 2\n" +
            "    LEFT JOIN jl_history_columns jhc14 ON\n" +
            "        jhc14.jrn_id = jlh.id\n" +
            "    AND\n" +
            "        jhc14.column_name = 'archived'\n" +
            "    AND\n" +
            "        jlh.obj_type = 2\n" +
            "WHERE\n" +
            "        CAST(jlh.object_id AS NUMERIC) IN (\n" +
            "            SELECT\n" +
            "                id\n" +
            "            FROM\n" +
            "                area_medical_employees ame\n" +
            "            WHERE\n" +
            "                ame.area_id = :areaId\n" +
            "        )\n" +
            "    AND\n" +
            "        jlh.obj_type = 1\n" +
            "    OR\n" +
            "        CAST(jlh.object_id AS NUMERIC) = :areaId\n" +
            "    AND\n" +
            "        jlh.obj_type = 2\n",
            nativeQuery = true)
    Page<AreaOrEmployeeEvent> findAreaAndEmployeeEvents(@Param("areaId") Long areaId, Pageable paging);
}
