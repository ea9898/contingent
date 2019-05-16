package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.Specialization;
import moscow.ptnl.contingent.area.entity.nsi.SpecializationToPositionNom;
import moscow.ptnl.contingent.area.entity.nsi.SpecializationToPositionNom_;
import moscow.ptnl.contingent.area.repository.BaseRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.List;

@Repository
@Transactional(propagation= Propagation.MANDATORY)
public class SpecializationToPositionNomRepositoryImpl extends BaseRepository implements SpecializationToPositionNomRepository {
    @Override
    public Specialization getSpecializationIdByPositionNomId(long positionNomClinicId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<SpecializationToPositionNom> criteria = criteriaBuilder.createQuery(SpecializationToPositionNom.class);
        Root<SpecializationToPositionNom> root = criteria.from(SpecializationToPositionNom.class);
        //Join<SpecializationToPositionNom, Specialization> join = root.join(SpecializationToPositionNom_.specialization, JoinType.LEFT);
        criteria.select(root);
        criteria.where(
                criteriaBuilder.equal(root.get(SpecializationToPositionNom_.positionNom), positionNomClinicId));
        List<SpecializationToPositionNom> results = entityManager.createQuery(criteria).getResultList();

        if (results.isEmpty()) {
            return null;
        } else {
            return results.get(0).getSpecialization(); //TODO не получается зделать FetchType.EAGER
        }
    }
}
