package moscow.ptnl.contingent.area.repository.nsi;

import moscow.ptnl.contingent.area.entity.nsi.AreaType_;
import moscow.ptnl.contingent.area.entity.nsi.MuTypeAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.MuTypeAreaTypes_;
import moscow.ptnl.contingent.area.entity.nsi.PolicyType;
import moscow.ptnl.contingent.area.entity.nsi.PolicyType_;
import moscow.ptnl.contingent.area.model.nsi.AvailableToCreateType;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

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
                root.get(PolicyType_.code).in(codes);

        return policyTypeCRUDRepository.findAll(specification);
    }
}
