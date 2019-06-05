package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees_;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.List;

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
                criteriaBuilder.equal(root.get(AreaMedicalEmployees_.replacement), false);
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

}
