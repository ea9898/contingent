package moscow.ptnl.contingent.area.transform;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.area.types.GetMuAvailableAreaTypesResponse;

import java.util.stream.Collectors;

@Component
public class GetMuAvailableAreaTypesResponseMapper implements Transform<GetMuAvailableAreaTypesResponse, moscow.ptnl.contingent.area.model.area.MuAreaTypesFull> {

    @Autowired
    private AreaTypeShortMapper areaTypeShortMapper;

    @Override
    public GetMuAvailableAreaTypesResponse entityToDtoTransform(moscow.ptnl.contingent.area.model.area.MuAreaTypesFull entity) {
        GetMuAvailableAreaTypesResponse response = new GetMuAvailableAreaTypesResponse();

        if (!entity.getUsedAreaTypes().isEmpty()) {
            response.setAvailableToUseAreaTypes(new GetMuAvailableAreaTypesResponse.AvailableToUseAreaTypes());
            response.getAvailableToUseAreaTypes().getAreaTypes().addAll(entity.getUsedAreaTypes().stream()
                    .map(areaTypeShortMapper::entityToDtoTransform)
                    .collect(Collectors.toList()));
        }
        if (!entity.getAvailableAreaTypes().isEmpty()) {
            response.setAvailableToAddAreaTypes(new GetMuAvailableAreaTypesResponse.AvailableToAddAreaTypes());
            response.getAvailableToAddAreaTypes().getAreaTypes().addAll(entity.getAvailableAreaTypes().stream()
                    .map(areaTypeShortMapper::entityToDtoTransform)
                    .collect(Collectors.toList()));
        }
        return response;
    }

    @Override
    public moscow.ptnl.contingent.area.model.area.MuAreaTypesFull dtoToEntityTransform(GetMuAvailableAreaTypesResponse dto) {
        return null;
    }
}
