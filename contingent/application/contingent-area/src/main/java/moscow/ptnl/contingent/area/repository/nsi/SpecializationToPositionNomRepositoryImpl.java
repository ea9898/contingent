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
    public Specialization getSpecializationIdByPositionNomId(long positionNomId) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<SpecializationToPositionNom> criteria = criteriaBuilder.createQuery(SpecializationToPositionNom.class);
        Root<SpecializationToPositionNom> template = criteria.from(SpecializationToPositionNom.class);
        criteria.where(
                criteriaBuilder.equal(template.get(SpecializationToPositionNom_.positionNom.getName()), positionNomId)
        );
        List<SpecializationToPositionNom> results = entityManager.createQuery(criteria).getResultList();

        return results.isEmpty() ? null : results.get(0).getSpecialization();
    }
}
