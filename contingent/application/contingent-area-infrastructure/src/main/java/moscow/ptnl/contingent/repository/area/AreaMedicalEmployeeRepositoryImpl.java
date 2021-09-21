package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees_;
import moscow.ptnl.contingent.domain.area.model.area.AreaHistory;
import moscow.ptnl.contingent.domain.area.repository.AreaMedicalEmployeeRepository;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.lang.reflect.Field;
import java.math.BigInteger;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import moscow.ptnl.contingent.domain.area.entity.Area_;
import moscow.ptnl.contingent.repository.CommonSpecification;

import javax.persistence.Tuple;

@Repository
@Transactional(propagation = Propagation.MANDATORY)
public class AreaMedicalEmployeeRepositoryImpl extends BaseRepository implements AreaMedicalEmployeeRepository {

    @Autowired
    AreaMedicalEmployeeCRUDRepository areaMedicalEmployeeCRUDRepository;

    private Specification<AreaMedicalEmployees> findAreasMedicalEmplyeesByAreaIdSpec(long areaId) {
        return (root, criteriaQuery, criteriaBuilder) ->
            criteriaBuilder.equal(root.get(AreaMedicalEmployees_.area), areaId);
    }

    private Specification<AreaMedicalEmployees> findAreasMedicalEmplyeesByAreaIdsSpec(List<Long> areaIds) {
        return (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.in(root.get(AreaMedicalEmployees_.area.getName()).get(Area_.id.getName())).value(areaIds); //root.get(AreaMedicalEmployees_.area).in(areaIds);
                
    }

    private Specification<AreaMedicalEmployees> findAreasMedicalEmplyeesByJobsIdsSpec(List<Long> jobIds) {
        return CommonSpecification.in(AreaMedicalEmployees_.medicalEmployeeJobId, jobIds);
        //return (root, criteriaQuery, criteriaBuilder) ->
        //        root.get(AreaMedicalEmployees_.medicalEmployeeJobId).in(jobIds);
    }

    private Specification<AreaMedicalEmployees> findAreasMedicalEmplyeesBySnilsSpec(List<String> snils) {
        return CommonSpecification.in(AreaMedicalEmployees_.snils, snils);
        //return (root, criteriaQuery, criteriaBuilder) ->
        //        root.get(AreaMedicalEmployees_.snils).in(snils);
    }

    private Specification<AreaMedicalEmployees> findAreasMedicalEmplyeesNotError() {
        return (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.or(
                        criteriaBuilder.isNull(root.get(AreaMedicalEmployees_.isError)),
                        criteriaBuilder.equal(root.get(AreaMedicalEmployees_.isError), false)
                );
    }

    private Specification<AreaMedicalEmployees> findAreasMedicalEmplyeesByAreasSpec(List<Area> areas) {
        List<Long> areasIds = areas.stream().map(a -> a.getId()).collect(Collectors.toList());
        return (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.in(root.get(AreaMedicalEmployees_.area.getName()).get(Area_.id.getName())).value(areasIds);
                //root.get(AreaMedicalEmployees_.area).in(areas);
    }

    private Specification<AreaMedicalEmployees> actualEmployeesSpec() {
        return (root, criteriaQuery, criteriaBuilder) ->
            criteriaBuilder.or(
                criteriaBuilder.isNull(root.get(AreaMedicalEmployees_.endDate)),
                criteriaBuilder.greaterThanOrEqualTo(root.get(AreaMedicalEmployees_.endDate.getName()), LocalDate.now())
            );
    }

    private Specification<AreaMedicalEmployees> mainEmployeesSpec() {
        return (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(AreaMedicalEmployees_.replacement), false);
    }

    private Specification<AreaMedicalEmployees> replacementEmployeesSpec() {
        return (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(AreaMedicalEmployees_.replacement), true);
    }

    private Specification<AreaMedicalEmployees> getEmployeesByJobIdSpec(long jobId) {
        return (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(AreaMedicalEmployees_.medicalEmployeeJobId), jobId);
    }

    @Override
    public List<AreaMedicalEmployees> getEmployeesByAreaId(long areaId) {
        return areaMedicalEmployeeCRUDRepository.findAll(findAreasMedicalEmplyeesByAreaIdSpec(areaId));
    }

    @Override
    public List<AreaMedicalEmployees> getEmployeesMainActualByAreaId(long areaId) {
        return areaMedicalEmployeeCRUDRepository.findAll(
                findAreasMedicalEmplyeesByAreaIdSpec(areaId)
                        .and(findAreasMedicalEmplyeesNotError())
                        .and(mainEmployeesSpec()).and(actualEmployeesSpec()));
    }

    @Override
    public List<AreaMedicalEmployees> getEmployeesReplacementActualByAreaId(long areaId) {
        return areaMedicalEmployeeCRUDRepository.findAll(
                findAreasMedicalEmplyeesByAreaIdSpec(areaId)
                        .and(findAreasMedicalEmplyeesNotError())
                        .and(replacementEmployeesSpec()).and(actualEmployeesSpec()));
    }

    @Override
    public Map<Area, List<AreaMedicalEmployees>> getEmployeesByAreaIds(List<Area> areas) {
        if (areas.isEmpty()) {
            return new HashMap<>();
        }
        return areaMedicalEmployeeCRUDRepository.findAll(
                findAreasMedicalEmplyeesByAreasSpec(areas)
                .and(actualEmployeesSpec())
                .and(findAreasMedicalEmplyeesNotError()))
                .stream().collect(Collectors.groupingBy(AreaMedicalEmployees::getArea, Collectors.toList()));
    }

    @Override
    public List<AreaMedicalEmployees> findEmployees(long jobId, Boolean replacement) {
        Specification<AreaMedicalEmployees> specification = getEmployeesByJobIdSpec(jobId);

        if (Boolean.TRUE.equals(replacement)) {
            specification = specification.and(replacementEmployeesSpec());
        }
        if (Boolean.FALSE.equals(replacement)) {
            specification = specification.and(mainEmployeesSpec());
        }
        return areaMedicalEmployeeCRUDRepository.findAll(specification);
    }

    @Override
    public List<Area> findAreasByEmployee(long jobId) {
        Specification<AreaMedicalEmployees> specification = getEmployeesByJobIdSpec(jobId);
        return areaMedicalEmployeeCRUDRepository.findAll(specification).
                stream().map(AreaMedicalEmployees::getArea).collect(Collectors.toList());
    }

    @Override
    public List<Area> findAreas(List<Long> areaIds, List<Long> jobIds, List<String> snils) {
        Specification<AreaMedicalEmployees> specification = findAreasMedicalEmplyeesNotError();
        if (areaIds != null && !areaIds.isEmpty()) {
            specification = specification.and(findAreasMedicalEmplyeesByAreaIdsSpec(areaIds));
        }
        if (jobIds != null && !jobIds.isEmpty()) {
            specification = specification.and(findAreasMedicalEmplyeesByJobsIdsSpec(jobIds));
        }
        if (snils != null && !snils.isEmpty()) {
            specification = specification.and(findAreasMedicalEmplyeesBySnilsSpec(snils));
        }
        return areaMedicalEmployeeCRUDRepository.findAll(specification)
            .stream().map(AreaMedicalEmployees::getArea).distinct().collect(Collectors.toList());
    }

    @Override
    public List<AreaMedicalEmployees> findAllById(List<Long> ids) {
        return areaMedicalEmployeeCRUDRepository.findAllById(ids);
    }

    @Override
    public List<AreaMedicalEmployees> saveAll(List<AreaMedicalEmployees> areaMedicalEmployees) {
        return areaMedicalEmployeeCRUDRepository.saveAll(areaMedicalEmployees);
    }

    @Override
    public void delete(AreaMedicalEmployees areaMedicalEmployees) {
        areaMedicalEmployeeCRUDRepository.delete(areaMedicalEmployees);
    }

    @Override
    public Page<AreaHistory.Event> areaHistory(Long areaId, Pageable paging) {
        return areaMedicalEmployeeCRUDRepository.areaHistory(areaId, paging);
    }
}
