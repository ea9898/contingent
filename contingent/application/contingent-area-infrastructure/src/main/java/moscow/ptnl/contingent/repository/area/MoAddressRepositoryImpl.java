package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.area.MoAddress_;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaType_;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class MoAddressRepositoryImpl extends BaseRepository implements MoAddressRepository {

    @Autowired
    private MoAddressPagingAndSortingRepository moAddressPagingAndSortingRepository;

    @Override
    public Page<MoAddress> getActiveMoAddresses(long moId, List<Long> areaTypeCodes, Pageable paging) {
        Specification<MoAddress> spec = (Specification<MoAddress>) (root, criteriaQuery, criteriaBuilder) ->
                criteriaBuilder.and(
                        criteriaBuilder.equal(root.get(MoAddress_.moId.getName()), moId),
                        areaTypeCodes == null || areaTypeCodes.isEmpty() ? criteriaBuilder.conjunction() :
                                root.get(MoAddress_.areaType.getName()).get(AreaType_.code.getName()).in(areaTypeCodes),
                        criteriaBuilder.or(
                                criteriaBuilder.greaterThanOrEqualTo(root.get(MoAddress_.endDate.getName()), LocalDate.now()),
                                root.get(MoAddress_.endDate.getName()).isNull()
                        )
                );
        if (paging == null) {
            return new PageImpl<>(moAddressPagingAndSortingRepository.findAll(spec, Sort.by(MoAddress_.id.getName()).ascending()));
        }
        return moAddressPagingAndSortingRepository.findAll(spec,
                PageRequest.of(paging.getPageNumber(), paging.getPageSize(), Sort.by(MoAddress_.id.getName()).ascending()));
    }

    @Override
    public List<MoAddress> getActiveMoAddresses(AreaType areaType) {
        Specification<MoAddress> moAddressSpecification = (root, criteriaQuery, criteriaBuilder) ->
            criteriaBuilder.and(
                criteriaBuilder.equal(root.get(MoAddress_.areaType.getName()), areaType.getCode()),
                    criteriaBuilder.or(
                            criteriaBuilder.greaterThanOrEqualTo(root.get(MoAddress_.endDate.getName()), LocalDate.now()),
                            root.get(MoAddress_.endDate.getName()).isNull()
            ));

        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification);
    }
}