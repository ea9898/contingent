package moscow.ptnl.contingent.area.transform;

import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;
import ru.mos.emias.contingent2.address.AddressRegistryBaseType;
import ru.mos.emias.contingent2.address.Area;
import ru.mos.emias.contingent2.address.AreaOMKTE;
import ru.mos.emias.contingent2.address.Building;
import ru.mos.emias.contingent2.address.City;
import ru.mos.emias.contingent2.address.Place;
import ru.mos.emias.contingent2.address.Plan;
import ru.mos.emias.contingent2.address.Region;
import ru.mos.emias.contingent2.address.RegionOMKTE;
import ru.mos.emias.contingent2.address.Street;

import java.util.List;
import java.util.Map;

@Mapper(componentModel="spring")
public interface AddressRegistryBaseTypeCloner {

    AddressRegistryBaseTypeCloner MAPPER = Mappers.getMapper( AddressRegistryBaseTypeCloner.class );

    AddressRegistryBaseType clone(AddressRegistryBaseType customerDto);

    Region clone(Region region);

    RegionOMKTE clone(RegionOMKTE regionOMKTE);

    Area clone(Area area);

    AreaOMKTE clone(AreaOMKTE areaOMKTE);

    City clone(City city);

    Place clone(Place place);

    Plan clone(Plan plan);

    Street clone(Street street);

    Building clone(Building building);
}
