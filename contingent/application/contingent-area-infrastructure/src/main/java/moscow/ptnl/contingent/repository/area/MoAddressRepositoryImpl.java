package moscow.ptnl.contingent.repository.area;

import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.Addresses_;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress_;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.domain.area.entity.MoAddress_;
import moscow.ptnl.contingent.domain.area.repository.MoAddressRepository;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
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
import java.util.Optional;

import moscow.ptnl.contingent.nsi.domain.area.AreaType_;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;

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
                            criteriaBuilder.in(root.get(MoAddress_.areaType.getName()).get(AreaType_.code.getName())).value(areaTypeCodes), //root.get(MoAddress_.areaType.getName()).get(AreaType_.code.getName()).in(areaTypeCodes),
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
        Specification<MoAddress> moAddressSpecification = (root, criteriaQuery, criteriaBuilder) -> {
            //Принудительная загрузка адресов (отмена Fetch.LAZY), для ускорения выборки
            root.fetch(MoAddress_.address, JoinType.INNER);

            return criteriaBuilder.and(
                    criteriaBuilder.equal(root.get(MoAddress_.areaType.getName()), areaType.getCode()),
                    criteriaBuilder.or(
                            criteriaBuilder.greaterThanOrEqualTo(root.get(MoAddress_.endDate.getName()), LocalDate.now()),
                            root.get(MoAddress_.endDate.getName()).isNull()
                    ));
        };
        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification);
    }

    @Override
    public List<MoAddress> getActiveMoAddressByGlobalId(Long globalId, AreaType areaType) {
        Specification<MoAddress> moAddressSpecification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<MoAddress, Addresses> addressesJoin = root.join(MoAddress_.address, JoinType.INNER);
            return criteriaBuilder.and(
                    criteriaBuilder.equal(addressesJoin.get(Addresses_.globalId.getName()), globalId),
                    criteriaBuilder.equal(root.get(MoAddress_.areaType.getName()), areaType.getCode()),
                    criteriaBuilder.or(
                            criteriaBuilder.greaterThanOrEqualTo(root.get(MoAddress_.endDate.getName()), LocalDate.now()),
                            root.get(MoAddress_.endDate.getName()).isNull()
                    ));
        };
        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification);
    }

    @Override
    public List<MoAddress> getActiveMoAddressByGlobalIdAndLevel(Long globalId, String aoLevel, AreaType areaType) {
        Specification<MoAddress> moAddressSpecification = (root, criteriaQuery, criteriaBuilder) -> {
            Join<MoAddress, Addresses> addressesJoin = root.join(MoAddress_.address, JoinType.INNER);
            return criteriaBuilder.and(
                    criteriaBuilder.or(
                            criteriaBuilder.equal(addressesJoin.get(Addresses_.globalId.getName()), globalId),
                            criteriaBuilder.notEqual(addressesJoin.get(Addresses_.aoLevel.getName()), aoLevel)
                    ),
                    criteriaBuilder.equal(root.get(MoAddress_.areaType.getName()), areaType.getCode()),
                    criteriaBuilder.or(
                            criteriaBuilder.greaterThanOrEqualTo(root.get(MoAddress_.endDate.getName()), LocalDate.now()),
                            root.get(MoAddress_.endDate.getName()).isNull()
                    ));
        };
        return moAddressPagingAndSortingRepository.findAll(moAddressSpecification);
    }

    @Override
    public Optional<MoAddress> findById(Long id) {
        return moAddressPagingAndSortingRepository.findById(id);
    }

    @Override
    public void delete(MoAddress moAddress) {
        moAddressPagingAndSortingRepository.delete(moAddress);
    }

    @Override
    public MoAddress save(MoAddress moAddress) {
        return moAddressPagingAndSortingRepository.save(moAddress);
    }

    @Override
    public List<MoAddress> saveAll(List<MoAddress> moAddress) {
        return moAddressPagingAndSortingRepository.saveAll(moAddress);
    }
}
