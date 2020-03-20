package moscow.ptnl.contingent.area.transform;

import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.address.AddressRegistryBaseType;
import ru.mos.emias.contingent2.address.Area;
import ru.mos.emias.contingent2.address.AreaOMKTE;
import ru.mos.emias.contingent2.address.City;
import ru.mos.emias.contingent2.address.Place;
import ru.mos.emias.contingent2.address.Plan;
import ru.mos.emias.contingent2.address.Street;

@Component
public class AddressRegistryBaseTypeMapper {

    public static AddressRegistryBaseType entityToDtoTransform(SearchAreaAddress entityObject) {
        AddressRegistryBaseType address = new AddressRegistryBaseType();
        address.setAoLevel(entityObject.getAoLevel());
        address.setGlobalIdNsi(entityObject.getGlobalIdNsi());
        Street street = new Street();
        street.setCode(entityObject.getStreetCode());
        address.setStreet(street);
        Plan plan = new Plan();
        plan.setCode(entityObject.getPlanCode());
        address.setPlan(plan);
        Place place = new Place();
        place.setCode(entityObject.getPlaceCode());
        address.setPlace(place);
        City city = new City();
        city.setCode(entityObject.getCityCode());
        address.setCity(city);
        Area area = new Area();
        area.setCode(entityObject.getAreaCode());
        address.setArea(area);
        AreaOMKTE areaOMKTE = new AreaOMKTE();
        address.setAreaOMKTE(areaOMKTE);
        return address;
    }
}