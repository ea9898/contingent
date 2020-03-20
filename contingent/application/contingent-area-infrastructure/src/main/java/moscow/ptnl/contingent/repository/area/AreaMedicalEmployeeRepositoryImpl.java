package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees_;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

@Repository
@Transactional(propagation = Propagation.MANDATORY)
public class AreaMedicalEmployeeRepositoryImpl extends BaseRepository implements AreaMedicalEmployeeRepository {

    @Autowired
    AreaMedicalEmployeeCRUDRepository areaMedicalEmployeeCRUDRepository;

    private Specification<AreaMedicalEmployees> findAreasMedicalEmplyeesByAreaIdSpec(long areaId) {
        return (root, criteriaQuery, criteriaBuilder) ->
            criteriaBuilder.equal(root.get(AreaMedicalEmployees_.area), areaId);
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
                        .and(mainEmployeesSpec()).and(actualEmployeesSpec()));
    }

    @Override
    public List<AreaMedicalEmployees> getEmployeesReplacementActualByAreaId(long areaId) {
        return areaMedicalEmployeeCRUDRepository.findAll(
                findAreasMedicalEmplyeesByAreaIdSpec(areaId)
                        .and(replacementEmployeesSpec()).and(actualEmployeesSpec()));
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
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<AreaMedicalEmployees> criteria = criteriaBuilder.createQuery(AreaMedicalEmployees.class);
        Root<AreaMedicalEmployees> root = criteria.from(AreaMedicalEmployees.class);
        criteria.select(root);
        criteria.where(
                criteriaBuilder.and(
                        areaIds == null || areaIds.isEmpty() ? criteriaBuilder.conjunction() :
                                root.get(AreaMedicalEmployees_.area).in(areaIds),
                        jobIds == null || jobIds.isEmpty() ? criteriaBuilder.conjunction() :
                                root.get(AreaMedicalEmployees_.medicalEmployeeJobId).in(jobIds),
                        snils == null || snils.isEmpty() ? criteriaBuilder.conjunction() :
                                root.get(AreaMedicalEmployees_.snils).in(snils))
        );
        return entityManager.createQuery(criteria).getResultList().
                stream().map(AreaMedicalEmployees::getArea).distinct().collect(Collectors.toList());
    }
}
