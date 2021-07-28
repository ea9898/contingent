package moscow.ptnl.contingent.area.transform.v2;

import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;

import org.mapstruct.AfterMapping;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;

import ru.mos.emias.contingent2.address.v2.AddressRegistryBaseType;
import ru.mos.emias.contingent2.address.v2.NameWithType;

@Mapper(componentModel="spring")
public interface AddressRegistryToAddressRegistryBaseMapperV2 {

    AddressRegistryToAddressRegistryBaseMapperV2 MAPPER = Mappers.getMapper(AddressRegistryToAddressRegistryBaseMapperV2.class);

    @Mappings({})
    AddressRegistryBaseType entityToDtoTransform(AddressRegistry entityObject);

    @Mappings({
            @Mapping(source = "globalId", target = "globalIdNsi"),
            @Mapping(source = "aoLevel", target = "aoLevel"),
            @Mapping(source = "address", target = "addressString"),
            @Mapping(source = "regionId", target = "region.id"),
            @Mapping(source = "regionCode", target = "region.code"),
            @Mapping(source = "regionName", target = "region.name"),
            @Mapping(source = "regionTeId", target = "regionOMKTE.id"),
            @Mapping(source = "regionTeCode", target = "regionOMKTE.code"),
            @Mapping(source = "regionTeName", target = "regionOMKTE.name"),
            @Mapping(source = "regionTeTypeName", target = "regionOMKTE.type.full"),
            @Mapping(source = "regionTeTypeNameShort", target = "regionOMKTE.type.short"),
            @Mapping(source = "areaTeId", target = "areaOMKTE.id"),
            @Mapping(source = "areaCodeOmkTe", target = "areaOMKTE.code"),
            @Mapping(source = "areaTeName", target = "areaOMKTE.name"),
            @Mapping(source = "areaTeTypeName", target = "areaOMKTE.type.full"),
            @Mapping(source = "areaTeTypeNameShort", target = "areaOMKTE.type.short"),
            @Mapping(source = "areaId", target = "area.id"),
            @Mapping(source = "areaCode", target = "area.code"),
            @Mapping(source = "areaBtiCode", target = "area.codeBTI"),
            @Mapping(source = "areaName", target = "area.name"),
            @Mapping(source = "areaTypeName", target = "area.type.full"),
            @Mapping(source = "areaTypeNameShort", target = "area.type.short"),
            @Mapping(source = "cityId", target = "city.id"),
            @Mapping(source = "cityCode", target = "city.code"),
            @Mapping(source = "cityBtiCode", target = "city.codeBTI"),
            @Mapping(source = "cityName", target = "city.name"),
            @Mapping(source = "cityTypeName", target = "city.type.full"),
            @Mapping(source = "cityTypeNameShort", target = "city.type.short"),
            @Mapping(source = "placeId", target = "place.id"),
            @Mapping(source = "placeCode", target = "place.code"),
            @Mapping(source = "placeBtiCode", target = "place.codeBTI"),
            @Mapping(source = "placeName", target = "place.name"),
            @Mapping(source = "placeTypeName", target = "place.type.full"),
            @Mapping(source = "placeTypeNameShort", target = "place.type.short"),
            @Mapping(source = "planId", target = "plan.id"),
            @Mapping(source = "planCode", target = "plan.code"),
            @Mapping(source = "planBtiCode", target = "plan.codeBTI"),
            @Mapping(source = "planName", target = "plan.name"),
            @Mapping(source = "planTypeName", target = "plan.type.full"),
            @Mapping(source = "planTypeNameShort", target = "plan.type.short"),
            @Mapping(source = "streetId", target = "street.id"),
            @Mapping(source = "streetCode", target = "street.code"),
            @Mapping(source = "streetBtiCode", target = "street.codeBTI"),
            @Mapping(source = "streetOmkUm", target = "street.codeOMKUM"),
            @Mapping(source = "streetName", target = "street.name"),
            @Mapping(source = "streetTypeName", target = "street.type.full"),
            @Mapping(source = "streetTypeNameShort", target = "street.type.short"),
            @Mapping(source = "l1Type", target = "building.house.type.full"),
            @Mapping(source = "l1TypeShort", target = "building.house.type.short"),
            @Mapping(source = "l1Value", target = "building.house.name"),
            @Mapping(source = "l2Type", target = "building.build.type.full"),
            @Mapping(source = "l2TypeShort", target = "building.build.type.short"),
            @Mapping(source = "l2Value", target = "building.build.name"),
            @Mapping(source = "l3Type", target = "building.construction.type.full"),
            @Mapping(source = "l3TypeShort", target = "building.construction.type.short"),
            @Mapping(source = "l3Value", target = "building.construction.name")
    })
    AddressRegistryBaseType entityToDtoTransform(Addresses entityObject);

    @Mappings({})
    AddressRegistry dtoToEntityTransform(AddressRegistryBaseType dtoObject);

    @AfterMapping
    default void after(final @MappingTarget AddressRegistryBaseType target, final Addresses source) {
        target.setArea(checkNameWithTypeEmpty(target.getArea()));
        target.setAreaOMKTE(checkNameWithTypeEmpty(target.getAreaOMKTE()));
        target.setCity(checkNameWithTypeEmpty(target.getCity()));
        target.setPlace(checkNameWithTypeEmpty(target.getPlace()));
        target.setPlan(checkNameWithTypeEmpty(target.getPlan()));
        target.setRegion(checkNameWithTypeEmpty(target.getRegion()));
        target.setRegionOMKTE(checkNameWithTypeEmpty(target.getRegionOMKTE()));
        target.setStreet(checkNameWithTypeEmpty(target.getStreet()));
        target.getBuilding().setBuild(checkNameWithTypeEmpty(target.getBuilding().getBuild()));
        target.getBuilding().setConstruction(checkNameWithTypeEmpty(target.getBuilding().getConstruction()));
        target.getBuilding().setHouse(checkNameWithTypeEmpty(target.getBuilding().getHouse()));

        if (target.getBuilding().getBuild() == null &&
                target.getBuilding().getConstruction() == null &&
                target.getBuilding().getHouse() == null) {
            target.setBuilding(null);
        }
    }

    default <T extends NameWithType> T checkNameWithTypeEmpty(T element) {
        if (element == null || element.getName() == null) {
            return null;
        }
        if (element.getType() != null && element.getType().getFull() == null) {
            element.setType(null);
        }
        return element;
    }
}
