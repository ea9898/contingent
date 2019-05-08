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
import java.time.LocalDate;
import java.util.List;

@Repository
@Transactional(propagation = Propagation.MANDATORY)
public class AreaMedicalEmployeeRepositoryImpl extends BaseRepository implements AreaMedicalEmployeeRepository {
    @Override
    public boolean isOtherMainEmployeeExist(long areaId, List<Long> deleteIds) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<AreaMedicalEmployee> criteria = criteriaBuilder.createQuery(AreaMedicalEmployee.class);
        Root<AreaMedicalEmployee> root = criteria.from(AreaMedicalEmployee.class);
        criteria.where(criteriaBuilder.and(
                criteriaBuilder.equal(root.get(AreaMedicalEmployee_.area).get(Area_.id), areaId),
                criteriaBuilder.equal(root.get(AreaMedicalEmployee_.replacement), false),
                criteriaBuilder.equal(root.get(AreaMedicalEmployee_.isDeleted), false),
                criteriaBuilder.not(root.get(AreaMedicalEmployee_.id).in(deleteIds)),
                criteriaBuilder.lessThanOrEqualTo(root.get(AreaMedicalEmployee_.startDate), LocalDate.now()),
                criteriaBuilder.or(
                        criteriaBuilder.greaterThan(root.get(AreaMedicalEmployee_.endDate), LocalDate.now()),
                        criteriaBuilder.isNull(root.get(AreaMedicalEmployee_.endDate)))));

        return !entityManager.createQuery(criteria).getResultList().isEmpty();
    }

    @Override
    public List<AreaMedicalEmployee> getMainEmployees(long areaId, List<Long> deleteIds) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<AreaMedicalEmployee> criteria = criteriaBuilder.createQuery(AreaMedicalEmployee.class);
        Root<AreaMedicalEmployee> root = criteria.from(AreaMedicalEmployee.class);
        criteria.where(criteriaBuilder.and(
                criteriaBuilder.equal(root.get(AreaMedicalEmployee_.area).get(Area_.id), areaId),
                criteriaBuilder.equal(root.get(AreaMedicalEmployee_.replacement), false),
                criteriaBuilder.equal(root.get(AreaMedicalEmployee_.isDeleted), false),
                criteriaBuilder.not(root.get(AreaMedicalEmployee_.id).in(deleteIds))))
                .orderBy(criteriaBuilder.asc(root.get(AreaMedicalEmployee_.startDate)));

        return entityManager.createQuery(criteria).getResultList();
    }

    @Override
    public List<Long> getJobIds(long areaId, List<Long> deleteIds) {
/*        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<AreaMedicalEmployee> criteria = criteriaBuilder.createQuery(AreaMedicalEmployee.class);
        Root<AreaMedicalEmployee> root = criteria.from(AreaMedicalEmployee.class);
        criteria.where(criteriaBuilder.and(
                criteriaBuilder.equal(root.get(AreaMedicalEmployee_.medicalEmployeeJobInfoId), medicalEmployeeJobInfoId),
                criteriaBuilder.equal(root.get(AreaMedicalEmployee_.isDeleted), false),
                criteriaBuilder.not(root.get(AreaMedicalEmployee_.id).in(deleteIds))))
                .orderBy(criteriaBuilder.asc(root.get(AreaMedicalEmployee_.startDate)));

        return entityManager.createQuery(criteria).getResultList();*/
        return null;
    }

    @Override
    public List<AreaMedicalEmployee> getEmployeesByJobInfoId(long medicalEmployeeJobInfoId, List<Long> deleteIds) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<AreaMedicalEmployee> criteria = criteriaBuilder.createQuery(AreaMedicalEmployee.class);
        Root<AreaMedicalEmployee> root = criteria.from(AreaMedicalEmployee.class);
        criteria.where(criteriaBuilder.and(
                criteriaBuilder.equal(root.get(AreaMedicalEmployee_.medicalEmployeeJobInfoId), medicalEmployeeJobInfoId),
                criteriaBuilder.equal(root.get(AreaMedicalEmployee_.isDeleted), false),
                criteriaBuilder.not(root.get(AreaMedicalEmployee_.id).in(deleteIds))))
                .orderBy(criteriaBuilder.asc(root.get(AreaMedicalEmployee_.startDate)));

        return entityManager.createQuery(criteria).getResultList();
    }
}
