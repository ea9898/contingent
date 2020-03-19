package moscow.ptnl.contingent.domain.area.methods;


import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.domain.area.CatalogDomainService;
import moscow.ptnl.contingent.domain.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.model.params.MoAvailableAreaTypesParams;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;

import javax.transaction.Transactional;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Transactional
public class AddMoAvailableAreaTypesMethod extends Method<MoAvailableAreaTypesParams, List<MoAvailableAreaTypes>> {

    protected List<AreaType> areaTypes;

    protected List<Long> areaTypeCodes;

    private CatalogDomainService catalogDomainService;

    public AddMoAvailableAreaTypesMethod(MoAvailableAreaTypesParams params) {
        super(params);
    }

    public AddMoAvailableAreaTypesMethod init(CatalogDomainService catalogDomainService) {
        this.catalogDomainService = catalogDomainService;
        return this;
    }

    private void checkAndGetAreaTypesExist() {

        areaTypeCodes.forEach(a -> {
            Optional<AreaType> areaType = catalogDomainService.get(a, AreaType.class);

            if (!areaType.isPresent() || Boolean.TRUE.equals(areaType.get().getArchived())) {
                validation.error(AreaErrorReason.AREA_TYPE_NOT_FOUND, new ValidationParameter("areaTypeCode", a));
            } else {
                areaTypes.add(areaType.get());
            }
        });
    }

    // К_УУ_1 2.
    // Система проверяет, что в списке доступных для МО отсутствует Тип участка с переданным кодом
    private void checkAreaTypesExistInMO() {
        List<AreaType> availableAreaTypes = catalogDomainService.findAreaTypesByMoId(params.getMoId()).stream()
                .map(MoAvailableAreaTypes::getAreaType)
                .collect(Collectors.toList());
        areaTypes.forEach(a -> {
            if (availableAreaTypes.contains(a)) {
                validation.error(AreaErrorReason.AREA_TYPE_ALREADY_EXISTS,
                        new ValidationParameter("areaTypeTitle", a.getTitle()));
            }
        });
    }

    @Override
    public Validation validate() {
        checkAndGetAreaTypesExist();
        checkAreaTypesExistInMO();
        return validation;
    }

    @Override
    public List<MoAvailableAreaTypes> execute() throws ContingentException {
        areaTypeCodes = params.getAreaTypeCodes().stream().distinct().collect(Collectors.toList());

        Validation validation = validate();
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        List<MoAvailableAreaTypes> moAvailableAreaTypesList = new ArrayList<>();
        areaTypeCodes.forEach(a -> {
            MoAvailableAreaTypes availableAreaType = new MoAvailableAreaTypes();
            areaTypes.stream()
                    .filter(t -> Objects.equals(t.getCode(), a))
                    .findFirst()
                    .ifPresent(availableAreaType::setAreaType);
            availableAreaType.setMoId(params.getMoId());
            availableAreaType.setCreateDate(LocalDateTime.now());
            moAvailableAreaTypesList.add(availableAreaType);
        });

        return moAvailableAreaTypesList;
    }
}
