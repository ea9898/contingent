package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.domain.area.model.area.MuAreaTypesFull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.area.types.GetMuAvailableAreaTypesResponse;

import java.util.stream.Collectors;

@Component
public class GetMuAvailableAreaTypesResponseMapper implements Transform<GetMuAvailableAreaTypesResponse, MuAreaTypesFull> {

    @Autowired
    private AreaTypeShortMapper areaTypeShortMapper;

    @Override
    public GetMuAvailableAreaTypesResponse entityToDtoTransform(MuAreaTypesFull entity) {
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
    public MuAreaTypesFull dtoToEntityTransform(GetMuAvailableAreaTypesResponse dto) {
        return null;
    }
}
