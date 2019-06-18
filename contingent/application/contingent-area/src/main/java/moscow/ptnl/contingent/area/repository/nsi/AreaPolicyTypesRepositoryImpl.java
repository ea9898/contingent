package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.AreaPolicyTypes;
import moscow.ptnl.contingent.area.entity.nsi.AreaPolicyTypes_;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.Root;
import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class AreaPolicyTypesRepositoryImpl extends BaseRepository implements AreaPolicyTypesRepository {

    @Autowired
    private AreaPolicyTypesCRUDRepository areaPolicyTypesCRUDRepository;

    @Override
    public List<AreaPolicyTypes> findAll(long areaId, long policyTypeCode) {
        Specification<AreaPolicyTypes> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(criteriaBuilder.equal(root.get(AreaPolicyTypes_.area_id), areaId),
                        criteriaBuilder.equal(root.get(AreaPolicyTypes_.policyTypeCode), policyTypeCode));
        return areaPolicyTypesCRUDRepository.findAll(specification);
    }

    @Override
    public void deleteAll(long areaId, List<Long> areaPolicyTypesDel) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaDelete<AreaPolicyTypes> criteria = criteriaBuilder.createCriteriaDelete(AreaPolicyTypes.class);
        Root<AreaPolicyTypes> root = criteria.from(AreaPolicyTypes.class);
        criteria.where(
                criteriaBuilder.and(
                                criteriaBuilder.equal(root.get(AreaPolicyTypes_.area_id), areaId ),
                                root.get(AreaPolicyTypes_.policyTypeCode).in(areaPolicyTypesDel))
        );
         entityManager.createQuery(criteria).executeUpdate();
    }
}
