package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees_;
import moscow.ptnl.contingent.repository.BaseRepository;
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
    public List<AreaMedicalEmployees> getEmployeesByAreaId(long areaId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<AreaMedicalEmployees> criteria = criteriaBuilder.createQuery(AreaMedicalEmployees.class);
        Root<AreaMedicalEmployees> root = criteria.from(AreaMedicalEmployees.class);
        criteria.where(criteriaBuilder.equal(root.get(AreaMedicalEmployees_.area), areaId));
        return entityManager.createQuery(criteria).getResultList();
    }

}
