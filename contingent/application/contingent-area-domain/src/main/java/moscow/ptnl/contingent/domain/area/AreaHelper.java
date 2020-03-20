package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.domain.area.entity.MoAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.repository.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
@Transactional
public class AreaHelper {

    @Autowired
    private MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    public List<AreaType> checkAndGetAreaTypesExist(List<Long> areaTypes, Validation validation) {
        List<AreaType> result = new ArrayList<>();

/*
        areaTypes.forEach(a -> {
            Optional<AreaType> areaType = catalogDomainService.get(a, AreaType.class);

            if (!areaType.isPresent() || Boolean.TRUE.equals(areaType.get().getArchived())) {
                validation.error(AreaErrorReason.AREA_TYPE_NOT_FOUND, new ValidationParameter("areaTypeCode", a));
            } else {
                result.add(areaType.get());
            }
        });
*/
        return result;
    }

    public void checkAreaTypesExistInMO(long moId, List<AreaType> areaTypes, Validation validation) {
        List<AreaType> availableAreaTypes = moAvailableAreaTypesRepository.findAreaTypes(moId).stream()
                .map(MoAvailableAreaTypes::getAreaType)
                .collect(Collectors.toList());
        areaTypes.forEach(a -> {
            if (availableAreaTypes.contains(a)) {
                validation.error(AreaErrorReason.AREA_TYPE_ALREADY_EXISTS,
                        new ValidationParameter("areaTypeTitle", a.getTitle()));
            }
        });
    }


}
