package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.repository.PolicyTypeRepository;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType_;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class PolicyTypeRepositoryImpl extends BaseRepository implements PolicyTypeRepository {

    @Autowired
    PolicyTypeCRUDRepository policyTypeCRUDRepository;

    @Override
    public List<PolicyType> findByIds(List<Long> codes) {
        if (codes.isEmpty()) {
            return new ArrayList<>();
        }
        Specification<PolicyType> specification = (root, criteriaQuery, criteriaBuilder) ->
            criteriaBuilder.in(root.get(PolicyType_.code.getName())).value(codes); //root.get(PolicyType_.code).in(codes);

        return policyTypeCRUDRepository.findAll(specification);
    }
}
