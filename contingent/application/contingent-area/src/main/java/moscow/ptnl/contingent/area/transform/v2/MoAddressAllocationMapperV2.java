package moscow.ptnl.contingent.area.transform.v2;

import moscow.ptnl.contingent.domain.area.model.area.MoAddressAllocation;
import moscow.ptnl.contingent.transform.Transform;

import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.v2.AddressAllocation;
import ru.mos.emias.contingent2.core.v2.AddressAllocation.MoAllocation;
import ru.mos.emias.contingent2.core.v2.AddressAllocation.MoAllocation.MoAddresses;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Component
public class MoAddressAllocationMapperV2 implements Transform<List<AddressAllocation>, List<MoAddressAllocation>> {

    @Autowired
    private AreaTypeShortMapperV2 areaTypeShortMapper;

    @Override
    public List<AddressAllocation> entityToDtoTransform(List<MoAddressAllocation> entities) {
        Map<Long, Map<Long, List<MoAddressAllocation>>> results = entities.stream()
                .collect(Collectors.groupingBy(m -> m.addressGlobalId, Collectors.groupingBy(m -> m.moId)));

        return results.entrySet().stream()
                .map(e -> map(e.getKey(), e.getValue()))
                .sorted(Comparator.comparing(AddressAllocation::getAddressGlobalId))
                .collect(Collectors.toList());
    }

    @Override
    public List<MoAddressAllocation> dtoToEntityTransform(List<AddressAllocation> dtoObject) {
        return null;
    }

    private AddressAllocation map(Long globalId, Map<Long, List<MoAddressAllocation>> addresses) {
        AddressAllocation address = new AddressAllocation();
        address.setAddressGlobalId(globalId);
        address.getMoAllocations().addAll(addresses.entrySet().stream()
                .map(e -> map(e.getKey(), e.getValue()))
                .sorted(Comparator.comparing(MoAllocation::getMoId))
                .collect(Collectors.toList()));

        return address;
    }

    private MoAllocation map(Long moId, List<MoAddressAllocation> addresses) {
        MoAllocation address = new MoAllocation();
        address.setMoId(moId);
        address.getMoAddresses().addAll(addresses.stream()
                .map(this::map)
                .sorted(Comparator.comparing(MoAddresses::getMoAddressId))
                .collect(Collectors.toList()));

        return address;
    }

    private MoAddresses map(MoAddressAllocation moAddressAllocation) {
        MoAddresses address = new MoAddresses();
        address.setMoAddressId(moAddressAllocation.moAddressId);
        address.setAreaType(areaTypeShortMapper.entityToDtoTransform(moAddressAllocation.areaType));

        return address;
    }
}
