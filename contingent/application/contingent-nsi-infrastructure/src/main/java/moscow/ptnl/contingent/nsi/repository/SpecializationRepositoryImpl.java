package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.area.Specialization;
import moscow.ptnl.contingent.nsi.domain.area.Specialization_;
import moscow.ptnl.contingent.nsi.domain.repository.SpecializationRepository;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation= Propagation.MANDATORY)
public class SpecializationRepositoryImpl extends BaseRepository implements SpecializationRepository {

    @Autowired
    private SpecializationCRUDRepository specializationCRUDRepository;

    @Override
    public Specialization getByCode(Long code) {
        Specification<Specialization> specification = (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.equal(root.get(Specialization_.code.getName()), code);

        return specializationCRUDRepository.findAll(specification).get(0);
    }
}
