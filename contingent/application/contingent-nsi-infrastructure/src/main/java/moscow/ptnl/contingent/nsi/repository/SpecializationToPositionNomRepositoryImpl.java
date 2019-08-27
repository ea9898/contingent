package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.area.entity.nsi.Specialization;

import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation= Propagation.MANDATORY)
public class SpecializationToPositionNomRepositoryImpl extends BaseRepository implements SpecializationToPositionNomRepository {
    @Override
    public Specialization getSpecializationIdByPositionNomId(long positionNomClinicId) {
        // TODO SpecializationToPositionNom больше нет
        return null;
//        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
//        CriteriaQuery<SpecializationToPositionNom> criteria = criteriaBuilder.createQuery(SpecializationToPositionNom.class);
//        Root<SpecializationToPositionNom> root = criteria.from(SpecializationToPositionNom.class);
//        //Join<SpecializationToPositionNom, Specialization> join = root.join(SpecializationToPositionNom_.specialization, JoinType.LEFT);
//        criteria.select(root);
//        criteria.where(
//                criteriaBuilder.equal(root.get(SpecializationToPositionNom_.positionNomClinic), positionNomClinicId));
//        List<SpecializationToPositionNom> results = entityManager.createQuery(criteria).getResultList();
//
//        if (results.isEmpty()) {
//            return null;
//        } else {
//            return results.get(0).getSpecialization(); //TODO не получается зделать FetchType.EAGER
//        }
    }
}
