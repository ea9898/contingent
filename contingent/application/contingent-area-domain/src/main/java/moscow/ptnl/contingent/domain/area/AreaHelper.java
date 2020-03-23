package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.domain.area.entity.MoAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.entity.MuAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.repository.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.repository.AreaTypesRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
@Transactional
public class AreaHelper {

    @Autowired
    private MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    @Autowired
    private AreaTypesRepository areaTypesRepository;

    public List<AreaType> checkAndGetAreaTypesExist(List<Long> areaTypes, Validation validation) {
        List<AreaType> result = new ArrayList<>();

        areaTypes.forEach(a -> {
            Optional<AreaType> areaType = areaTypesRepository.findById (a);

            if (!areaType.isPresent() || Boolean.TRUE.equals(areaType.get().getArchived())) {
                validation.error(AreaErrorReason.AREA_TYPE_NOT_FOUND, new ValidationParameter("areaTypeCode", a));
            } else {
                result.add(areaType.get());
            }
        });

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

    // К_УУ_2 2.
    // Система проверяет, что тип участка не присутствует ни в одном списке доступных для МУ
    public void checkAndGetAreaTypesNotExistInMU(List<MoAvailableAreaTypes> moAvailableAreaTypes, List<Long> areaTypeCodes, Validation validation) {
        Map<AreaType, List<MuAvailableAreaTypes>> found = moAvailableAreaTypes.stream()
                .filter(a -> a.getMuAvailableAreaTypes() != null)
                .flatMap(a -> a.getMuAvailableAreaTypes().stream())
                .filter(a -> areaTypeCodes.contains(a.getAreaType().getCode()))
                .collect(Collectors.groupingBy(MuAvailableAreaTypes::getAreaType));

        found.forEach((t, a) -> validation.error(AreaErrorReason.CANT_DELETE_AREA_TYPE,
                new ValidationParameter("areaTypeCode", t.getCode()),
                new ValidationParameter("muId", a.stream()
                        .map(MuAvailableAreaTypes::getMuId)
                        .map(String::valueOf)
                        .distinct()
                        .collect(Collectors.joining(", ")))
                )
        );
    }

    // К_УУ_2 1., К_УУ_4 1.
    // Система проверяет, что в списке доступных для МО присутствует Тип участка с переданным кодом
    public List<MoAvailableAreaTypes> checkAndGetAreaTypesNotExistInMO(long moId, List<Long> areaTypeCodes, Validation validation) {
        List<MoAvailableAreaTypes> moAvailableAreaTypes = moAvailableAreaTypesRepository.findAreaTypes(moId);
        List<Long> availableAreaTypes = moAvailableAreaTypes.stream()
                .map(MoAvailableAreaTypes::getAreaType)
                .map(AreaType::getCode)
                .collect(Collectors.toList());
        areaTypeCodes.forEach(a -> {
            if (!availableAreaTypes.contains(a)) {
                validation.error(AreaErrorReason.AREA_TYPE_NOT_EXISTS_IN_MO,
                        new ValidationParameter("areaTypeTitle", areaTypesRepository.findById(a).map(AreaType::getTitle).orElse(String.valueOf(a))));
            }
        });
        return moAvailableAreaTypes.stream()
                .filter(a -> areaTypeCodes.contains(a.getAreaType().getCode()))
                .collect(Collectors.toList());
    }

}
