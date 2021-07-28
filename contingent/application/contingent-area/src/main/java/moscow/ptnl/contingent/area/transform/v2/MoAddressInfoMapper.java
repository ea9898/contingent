package moscow.ptnl.contingent.area.transform.v2;

import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.model.area.MoAddressAllocation;
import moscow.ptnl.contingent.domain.area.model.area.MoAddressWithAddresses;
import moscow.ptnl.contingent.transform.Transform;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.v2.AddressAllocation;
import ru.mos.emias.contingent2.core.v2.AddressAllocation.MoAllocation;
import ru.mos.emias.contingent2.core.v2.AddressAllocation.MoAllocation.MoAddresses;
import ru.mos.emias.contingent2.core.v2.AddressInfo;
import ru.mos.emias.contingent2.core.v2.AreaAddressInfo;
import ru.mos.emias.contingent2.core.v2.MoAddressInfo;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

@Component
public class MoAddressInfoMapper implements Transform<List<AddressInfo>, List<MoAddressWithAddresses>> {

    @Autowired
    private AddressAllocationOrder2MapperV2 addressAllocationOrderMapper;

    @Autowired
    private AddressRegistryToAddressRegistryBaseMapperV2 addressRegistryToAddressRegistryBaseMapper;

    @Autowired
    private AreaTypeShortMapperV2 areaTypeShortMapper;

    @Override
    public List<AddressInfo> entityToDtoTransform(List<MoAddressWithAddresses> entities) {
        Map<Addresses, Map<Long, List<MoAddressWithAddresses>>> results = entities.stream()
                .collect(Collectors.groupingBy(m -> m.address, Collectors.groupingBy(m -> m.moAddress.getMoId())));

        return results.entrySet().stream()
                .map(e -> map(e.getKey(), e.getValue()))
                .sorted(Comparator.comparing(a -> a.getAddress() == null ? Long.MIN_VALUE : a.getAddress().getGlobalIdNsi(),
                        Comparator.nullsFirst(Comparator.naturalOrder())))
                .collect(Collectors.toList());
    }

    @Override
    public List<MoAddressWithAddresses> dtoToEntityTransform(List<AddressInfo> dtoObject) {
        return null;
    }

    private AddressInfo map(Addresses address, Map<Long, List<MoAddressWithAddresses>> moAddresses) {
        AddressInfo addressInfo = new AddressInfo();
        addressInfo.setAddress(addressRegistryToAddressRegistryBaseMapper.entityToDtoTransform(address));
        addressInfo.getMoInfos().addAll(moAddresses.entrySet().stream()
                .map(a -> map(a.getKey(), a.getValue()))
                .sorted(Comparator.comparing(AddressInfo.MoInfo::getMoId))
                .collect(Collectors.toList()));

        return addressInfo;
    }

    private AddressInfo.MoInfo map(long moId, List<MoAddressWithAddresses> moAddresses) {
        AddressInfo.MoInfo moInfo = new AddressInfo.MoInfo();
        moInfo.setMoId(moId);
        moInfo.getMoAddressInfos().addAll(moAddresses.stream()
                .map(this::map)
                .sorted(Comparator.comparing((MoAddressInfo a) -> a.getAreaType() == null ? Long.MAX_VALUE : a.getAreaType().getCode())
                        .thenComparing(MoAddressInfo::getStartDate, Comparator.nullsFirst(Comparator.naturalOrder()))
                        .thenComparing(MoAddressInfo::getMoAddressId))
                .collect(Collectors.toList()));

        return moInfo;
    }

    private MoAddressInfo map(MoAddressWithAddresses address) {
        MoAddressInfo addressInfo = new MoAddressInfo();
        addressInfo.setMoAddressId(address.moAddress.getId());
        addressInfo.setAreaType(areaTypeShortMapper.entityToDtoTransform(address.areaType));
        addressInfo.setAllocationOrder(addressAllocationOrderMapper.entityToDtoTransform(address.addressAllocationOrder));
        addressInfo.setRejectOrder(addressAllocationOrderMapper.entityToDtoTransform(address.addressRejectOrder));
        addressInfo.setStartDate(address.moAddress.getStartDate());
        addressInfo.setEndDate(address.moAddress.getEndDate());
        addressInfo.getAreaAddressInfos().addAll(address.areaAddresses.stream()
                .map(this::map)
                .sorted(Comparator.comparing(AreaAddressInfo::getStartDate, Comparator.nullsFirst(Comparator.naturalOrder()))
                        .thenComparing(AreaAddressInfo::getAreaAddressId))
                .collect(Collectors.toList()));

        return addressInfo;
    }

    private AreaAddressInfo map(AreaAddress address) {
        AreaAddressInfo addressInfo = new AreaAddressInfo();
        addressInfo.setAreaAddressId(address.getId());
        addressInfo.setAreaId(address.getArea().getId());
        addressInfo.setStartDate(address.getStartDate());
        addressInfo.setEndDate(address.getEndDate());

        return addressInfo;
    }
}
