package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployee;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployee_;
import moscow.ptnl.contingent.area.entity.area.Area_;
import moscow.ptnl.contingent.area.repository.BaseRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.List;

@Repository
@Transactional(propagation = Propagation.MANDATORY)
public class AreaMedicalEmployeeRepositoryImpl extends BaseRepository implements AreaMedicalEmployeeRepository {

    @Override
    public List<AreaMedicalEmployee> getMainEmployees(long areaId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<AreaMedicalEmployee> criteria = criteriaBuilder.createQuery(AreaMedicalEmployee.class);
        Root<AreaMedicalEmployee> root = criteria.from(AreaMedicalEmployee.class);
        criteria.where(criteriaBuilder.and(
                criteriaBuilder.equal(root.get(AreaMedicalEmployee_.area).get(Area_.id), areaId),
                criteriaBuilder.equal(root.get(AreaMedicalEmployee_.replacement), 0)))
                .orderBy(criteriaBuilder.asc(root.get(AreaMedicalEmployee_.startDate)));

        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<AreaMedicalEmployee> getReplacementEmployees(long areaId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<AreaMedicalEmployee> criteria = criteriaBuilder.createQuery(AreaMedicalEmployee.class);
        Root<AreaMedicalEmployee> root = criteria.from(AreaMedicalEmployee.class);
        criteria.where(criteriaBuilder.and(
                criteriaBuilder.equal(root.get(AreaMedicalEmployee_.area).get(Area_.id), areaId),
                criteriaBuilder.equal(root.get(AreaMedicalEmployee_.replacement), 1)))
                .orderBy(criteriaBuilder.asc(root.get(AreaMedicalEmployee_.startDate)));

        return entityManager.createQuery(criteria).getResultList();
    }


    @Override
    public List<AreaMedicalEmployee> getEmployeesByAreaId(long areaId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<AreaMedicalEmployee> criteria = criteriaBuilder.createQuery(AreaMedicalEmployee.class);
        Root<AreaMedicalEmployee> root = criteria.from(AreaMedicalEmployee.class);
        criteria.where(criteriaBuilder.equal(root.get(AreaMedicalEmployee_.area).get(Area_.id), areaId))
                .orderBy(criteriaBuilder.asc(root.get(AreaMedicalEmployee_.medicalEmployeeJobInfoId)),
                        criteriaBuilder.asc(root.get(AreaMedicalEmployee_.startDate)));
        return entityManager.createQuery(criteria).getResultList();
    }

}
