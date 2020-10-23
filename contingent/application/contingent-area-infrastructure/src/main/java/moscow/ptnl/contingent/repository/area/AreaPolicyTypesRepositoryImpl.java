package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.Area;

import moscow.ptnl.contingent.domain.area.entity.AreaPolicyTypes_;
import moscow.ptnl.contingent.domain.area.repository.AreaPolicyTypesRepository;
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
import java.util.stream.Collectors;
import moscow.ptnl.contingent.domain.area.entity.AreaPolicyTypes;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType_;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class AreaPolicyTypesRepositoryImpl extends BaseRepository implements AreaPolicyTypesRepository {

    @Autowired
    private AreaPolicyTypesCRUDRepository areaPolicyTypesCRUDRepository;

    @Override
    public List<AreaPolicyTypes> findAll(Area area, PolicyType policyType) {
        Specification<AreaPolicyTypes> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(criteriaBuilder.equal(root.get(AreaPolicyTypes_.area), area),
                        criteriaBuilder.equal(root.get(AreaPolicyTypes_.policyType), policyType));
        return areaPolicyTypesCRUDRepository.findAll(specification);
    }
    
    @Override
    public List<AreaPolicyTypes> findAll(Area area, List<PolicyType> policyTypes) {
        List<Long> policyTypesCodes = policyTypes.stream().map(p -> p.getCode()).collect(Collectors.toList());
        Specification<AreaPolicyTypes> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                        criteriaBuilder.equal(root.get(AreaPolicyTypes_.area), area),
                        criteriaBuilder.in(root.get(AreaPolicyTypes_.policyType.getName()).get(PolicyType_.code.getName())).value(policyTypesCodes) //root.get(AreaPolicyTypes_.policyType).in(policyTypes)                       
                );
        
        return areaPolicyTypesCRUDRepository.findAll(specification);
    }

    @Override
    public void deleteAll(Area area, List<PolicyType> areaPolicyTypesDel) {
        List<Long> policyTypesCodes = areaPolicyTypesDel.stream().map(p -> p.getCode()).collect(Collectors.toList());
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaDelete<AreaPolicyTypes> criteria = criteriaBuilder.createCriteriaDelete(AreaPolicyTypes.class);
        Root<AreaPolicyTypes> root = criteria.from(AreaPolicyTypes.class);
        criteria.where(
                criteriaBuilder.and(
                    criteriaBuilder.equal(root.get(AreaPolicyTypes_.area), area),
                    criteriaBuilder.in(root.get(AreaPolicyTypes_.policyType.getName()).get(PolicyType_.code.getName())).value(policyTypesCodes) //root.get(AreaPolicyTypes_.policyType).in(areaPolicyTypesDel)
                )
        );
         entityManager.createQuery(criteria).executeUpdate();
    }

    @Override
    public List<AreaPolicyTypes> saveAll(List<AreaPolicyTypes> areaPolicyTypes) {
        return areaPolicyTypesCRUDRepository.saveAll(areaPolicyTypes);
    }

    @Override
    public AreaPolicyTypes save(AreaPolicyTypes areaPolicyTypes) {
        return areaPolicyTypesCRUDRepository.save(areaPolicyTypes);
    }
}
