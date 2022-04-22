package moscow.ptnl.contingent.area.transform.v3;

import moscow.ptnl.contingent.domain.area.entity.MuAvailableAreaTypes;
import moscow.ptnl.contingent.transform.Transform;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.v3.AreaTypeShort;
import ru.mos.emias.contingent2.core.v3.MuAvailableAreaType;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Component
public class MuAvailableAreaTypesInMoMapper implements Transform<List<MuAvailableAreaType>, List<MuAvailableAreaTypes>> {

    @Autowired
    private AreaTypeShortMapperV3 areaTypeShortMapper;

    @Override
    public List<MuAvailableAreaType> entityToDtoTransform(List<MuAvailableAreaTypes> entities) {
        Map<Long, List<MuAvailableAreaTypes>> groupedResults = entities.stream()
                .collect(Collectors.groupingBy(MuAvailableAreaTypes::getMoId));
        List<MuAvailableAreaType> results = groupedResults.entrySet().stream()
                .map(e -> {
                    MuAvailableAreaType response = new MuAvailableAreaType();
                    response.setMoId(e.getKey());
                    response.getMuInfos().addAll(map(e.getValue()));
                    return response;
                })
                .sorted(Comparator.comparing(MuAvailableAreaType::getMoId))
                .collect(Collectors.toList());
        return results;
    }

    public List<MuAvailableAreaType.MuInfo> map(List<MuAvailableAreaTypes> entities) {
        Map<Long, List<MuAvailableAreaTypes>> groupedResults = entities.stream()
                .collect(Collectors.groupingBy(MuAvailableAreaTypes::getMuId));
        List<MuAvailableAreaType.MuInfo> results = groupedResults.entrySet().stream()
                .map(e -> {
                    MuAvailableAreaType.MuInfo response = new MuAvailableAreaType.MuInfo();
                    response.setMuId(e.getKey());
                    response.getAreaTypes().addAll(e.getValue().stream()
                            .map(MuAvailableAreaTypes::getAreaType)
                            .map(areaTypeShortMapper::entityToDtoTransform)
                            .sorted(Comparator.comparing(AreaTypeShort::getCode))
                            .collect(Collectors.toList()));
                    return response;
                })
                .sorted(Comparator.comparing(MuAvailableAreaType.MuInfo::getMuId))
                .collect(Collectors.toList());
        return results;
    }

    @Override
    public List<MuAvailableAreaTypes> dtoToEntityTransform(List<MuAvailableAreaType> dtoObject) {
        return null;
    }
}
