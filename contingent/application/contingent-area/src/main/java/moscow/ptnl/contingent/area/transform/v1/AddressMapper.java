package moscow.ptnl.contingent.area.transform.v1;

import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.domain.area.model.area.AddressRegistry;
import org.mapstruct.AfterMapping;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;
import org.mapstruct.Mappings;

@Mapper(componentModel="spring")
public abstract class AddressMapper {

    @Mappings({
            @Mapping(target="globalId", source="globalIdNsi"), //+
            @Mapping(target="address", source="addressString"), //+

            @Mapping(target = "regionId", source = "region.id"), //+
            @Mapping(target = "regionCode", source = "region.code"), //+
            @Mapping(target = "regionName", source = "region.name"), //+
            @Mapping(target = "regionTypename", source = "region.type.full"), //+

            @Mapping(target = "regionTeId", source = "regionOMKTE.id"), //+
            @Mapping(target = "regionTeCode", source = "regionOMKTE.code"), //+
            @Mapping(target = "regionTeName", source = "regionOMKTE.name"), //+
            @Mapping(target = "regionTeTypeName", source = "regionOMKTE.type.full"), //+
            @Mapping(target = "regionTeTypeNameShort", source = "regionOMKTE.type.short"), //+

            @Mapping(target = "areaCodeOmkTe", source = "areaOMKTE.code"), //+
            @Mapping(target = "areaTeName", source = "areaOMKTE.name"), //+
            @Mapping(target = "areaTeTypeName", source = "areaOMKTE.type.full"), //+
            @Mapping(target = "areaTeTypeNameShort", source = "areaOMKTE.type.short"),

            @Mapping(target = "areaCode", source = "area.code"), //+
            @Mapping(target = "areaBtiCode", source = "area.codeBTI"), //+
            @Mapping(target = "areaName", source = "area.name"), //+
            @Mapping(target = "areaTypeName", source = "area.type.full"), //+
            @Mapping(target = "areaTypeNameShort", source = "area.type.short"),

            @Mapping(target = "cityCode", source = "city.code"), //+
            @Mapping(target = "cityBtiCode", source = "city.codeBTI"), //+
            @Mapping(target = "cityName", source = "city.name"), //+
            @Mapping(target = "cityTypeName", source = "city.type.full"), //+
            @Mapping(target = "cityTypeNameShort", source = "city.type.short"),

            @Mapping(target = "placeCode", source = "place.code"), //+
            @Mapping(target = "placeBtiCode", source = "place.codeBTI"), //+
            @Mapping(target = "placeName", source = "place.name"), //+
            @Mapping(target = "placeTypeName", source = "place.type.full"), //+
            @Mapping(target = "placeTypeNameShort", source = "place.type.short"),

            @Mapping(target = "planCode", source = "plan.code"), //+
            @Mapping(target = "planBtiCode", source = "plan.codeBTI"), //+
            @Mapping(target = "planName", source = "plan.name"), //+
            @Mapping(target = "planTypeName", source = "plan.type.full"), //+
            @Mapping(target = "planTypeNameShort", source = "plan.type.short"),

            @Mapping(target = "streetCode", source = "street.code"), //+
            @Mapping(target = "streetBtiCode", source = "street.codeBTI"), //+
            @Mapping(target = "streetName", source = "street.name"), //+
            @Mapping(target = "streetTypeName", source = "street.type.full"), //+
            @Mapping(target = "streetTypeNameShort", source = "street.type.short"),
            @Mapping(target = "streetOmkUm", source = "street.codeOMKUM"), //+

            @Mapping(target = "l1Type", source = "building.house.type.full"), //+
            @Mapping(target = "l1TypeShort", source = "building.house.type.short"),
            @Mapping(target = "l1Value", source = "building.house.name"), //+

            @Mapping(target = "l2Type", source = "building.build.type.full"), //+
            @Mapping(target = "l2TypeShort", source = "building.build.type.short"),
            @Mapping(target = "l2Value", source = "building.build.name"), //+

            @Mapping(target = "l3Type", source = "building.construction.type.full"), //+
            @Mapping(target = "l3TypeShort", source = "building.construction.type.short"),
            @Mapping(target = "l3Value", source = "building.construction.name"), //+
            @Mapping(target = "updateDate", expression = "java( java.time.LocalDateTime.now() )"),

            @Mapping(target = "areaTeId", source = "areaOMKTE.id"),
            @Mapping(target = "areaId", source = "area.id"),
            @Mapping(target = "cityId", source = "city.id"),
            @Mapping(target = "placeId", source = "place.id"),
            @Mapping(target = "planId", source = "plan.id"),
            @Mapping(target = "streetId", source = "street.id"),

            @Mapping(target = "createDate", expression = "java( java.time.LocalDateTime.now() )")
    })
    public abstract Addresses dtoToEntityTransform(AddressRegistry addressRegistry);

    @Mappings({
            @Mapping(source="globalId", target="globalIdNsi"),
            @Mapping(source="address", target="addressString"),

            @Mapping(source = "regionId", target = "region.id"),
            @Mapping(source = "regionCode", target = "region.code"),
            @Mapping(source = "regionName", target = "region.name"),
            @Mapping(source = "regionTypename", target = "region.type.full"),

            @Mapping(source = "regionTeId", target = "regionOMKTE.id"),
            @Mapping(source = "regionTeCode", target = "regionOMKTE.code"),
            @Mapping(source = "regionTeName", target = "regionOMKTE.name"),
            @Mapping(source = "regionTeTypeName", target = "regionOMKTE.type.full"),
            @Mapping(source = "regionTeTypeNameShort", target = "regionOMKTE.type.short"),

            @Mapping(source = "areaCodeOmkTe", target = "areaOMKTE.code"),
            @Mapping(source = "areaTeName", target = "areaOMKTE.name"),
            @Mapping(source = "areaTeTypeName", target = "areaOMKTE.type.full"),
            @Mapping(source = "areaTeTypeNameShort", target = "areaOMKTE.type.short"),

            @Mapping(source = "areaCode", target = "area.code"),
            @Mapping(source = "areaBtiCode", target = "area.codeBTI"),
            @Mapping(source = "areaName", target = "area.name"),
            @Mapping(source = "areaTypeName", target = "area.type.full"),
            @Mapping(source = "areaTypeNameShort", target = "area.type.short"),

            @Mapping(source = "cityCode", target = "city.code"),
            @Mapping(source = "cityBtiCode", target = "city.codeBTI"),
            @Mapping(source = "cityName", target = "city.name"),
            @Mapping(source = "cityTypeName", target = "city.type.full"),
            @Mapping(source = "cityTypeNameShort", target = "city.type.short"),

            @Mapping(source = "placeCode", target = "place.code"),
            @Mapping(source = "placeBtiCode", target = "place.codeBTI"),
            @Mapping(source = "placeName", target = "place.name"),
            @Mapping(source = "placeTypeName", target = "place.type.full"),
            @Mapping(source = "placeTypeNameShort", target = "place.type.short"),

            @Mapping(source = "planCode", target = "plan.code"),
            @Mapping(source = "planBtiCode", target = "plan.codeBTI"),
            @Mapping(source = "planName", target = "plan.name"),
            @Mapping(source = "planTypeName", target = "plan.type.full"),
            @Mapping(source = "planTypeNameShort", target = "plan.type.short"),

            @Mapping(source = "streetCode", target = "street.code"),
            @Mapping(source = "streetBtiCode", target = "street.codeBTI"),
            @Mapping(source = "streetName", target = "street.name"),
            @Mapping(source = "streetTypeName", target = "street.type.full"),
            @Mapping(source = "streetTypeNameShort", target = "street.type.short"),
            @Mapping(source = "streetOmkUm", target = "street.codeOMKUM"),

            @Mapping(source = "l1Type", target = "building.house.type.full"),
            @Mapping(source = "l1TypeShort", target = "building.house.type.short"),
            @Mapping(source = "l1Value", target = "building.house.name"),

            @Mapping(source = "l2Type", target = "building.build.type.full"),
            @Mapping(source = "l2TypeShort", target = "building.build.type.short"),
            @Mapping(source = "l2Value", target = "building.build.name"),

            @Mapping(source = "l3Value", target = "building.construction.name"),
            @Mapping(source = "l3Type", target = "building.construction.type.full"),
            @Mapping(source = "l3TypeShort", target = "building.construction.type.short"),

            @Mapping(source = "areaTeId", target = "areaOMKTE.id"),
            @Mapping(source = "areaId", target = "area.id"),
            @Mapping(source = "cityId", target = "city.id"),
            @Mapping(source = "placeId", target = "place.id"),
            @Mapping(source = "planId", target = "plan.id"),
            @Mapping(source = "streetId", target = "street.id")
    })
    public abstract AddressRegistry entityToDtoTransform(Addresses address);

    @AfterMapping
    public AddressRegistry doAfterMapping(@MappingTarget AddressRegistry addressRegistryBaseType) {
        if (addressRegistryBaseType.getRegion().getCode() == null && addressRegistryBaseType.getRegion().getName() == null) {
            addressRegistryBaseType.setRegion(null);
        }
        if (addressRegistryBaseType.getRegionOMKTE().getCode() == null && addressRegistryBaseType.getRegionOMKTE().getName() == null) {
            addressRegistryBaseType.setRegionOMKTE(null);
        }
        if (addressRegistryBaseType.getAreaOMKTE().getCode() == null && addressRegistryBaseType.getAreaOMKTE().getName() == null) {
            addressRegistryBaseType.setAreaOMKTE(null);
        }
        if (addressRegistryBaseType.getArea().getCode() == null && addressRegistryBaseType.getArea().getName() == null) {
            addressRegistryBaseType.setArea(null);
        }
        if (addressRegistryBaseType.getCity().getCode() == null && addressRegistryBaseType.getCity().getName() == null) {
            addressRegistryBaseType.setCity(null);
        }
        if (addressRegistryBaseType.getPlan().getCode() == null && addressRegistryBaseType.getPlan().getName() == null) {
            addressRegistryBaseType.setPlan(null);
        }
        if (addressRegistryBaseType.getPlace().getCode() == null && addressRegistryBaseType.getPlace().getName() == null) {
            addressRegistryBaseType.setPlace(null);
        }
        if (addressRegistryBaseType.getStreet().getCode() == null && addressRegistryBaseType.getStreet().getName() == null) {
            addressRegistryBaseType.setStreet(null);
        }
        if (addressRegistryBaseType.getBuilding().getConstruction().getName() == null) {
            addressRegistryBaseType.getBuilding().setConstruction(null);
        }
        if (addressRegistryBaseType.getBuilding().getHouse().getName() == null) {
            addressRegistryBaseType.getBuilding().setHouse(null);
        }
        if (addressRegistryBaseType.getBuilding().getBuild().getName() == null) {
            addressRegistryBaseType.getBuilding().setBuild(null);
        }
        if (addressRegistryBaseType.getRegion() != null && addressRegistryBaseType.getRegion().getType() != null
                && addressRegistryBaseType.getRegion().getType().getFull() == null) {
            addressRegistryBaseType.getRegion().setType(null);
        }
        if (addressRegistryBaseType.getRegionOMKTE() != null && addressRegistryBaseType.getRegionOMKTE().getType() != null
                && addressRegistryBaseType.getRegionOMKTE().getType().getFull() == null) {
            addressRegistryBaseType.getRegionOMKTE().setType(null);
        }
        if (addressRegistryBaseType.getArea() != null && addressRegistryBaseType.getArea().getType() != null
                && addressRegistryBaseType.getArea().getType().getFull() == null) {
            addressRegistryBaseType.getArea().setType(null);
        }
        if (addressRegistryBaseType.getAreaOMKTE() != null && addressRegistryBaseType.getAreaOMKTE().getType() != null
                && addressRegistryBaseType.getAreaOMKTE().getType().getFull() == null) {
            addressRegistryBaseType.getAreaOMKTE().setType(null);
        }
        if (addressRegistryBaseType.getCity() != null && addressRegistryBaseType.getCity().getType() != null
                && addressRegistryBaseType.getCity().getType().getFull() == null) {
            addressRegistryBaseType.getCity().setType(null);
        }
        if (addressRegistryBaseType.getPlace() != null && addressRegistryBaseType.getPlace().getType() != null
                && addressRegistryBaseType.getPlace().getType().getFull() == null) {
            addressRegistryBaseType.getPlace().setType(null);
        }
        if (addressRegistryBaseType.getPlan() != null && addressRegistryBaseType.getPlan().getType() != null
                && addressRegistryBaseType.getPlan().getType().getFull() == null) {
            addressRegistryBaseType.getPlan().setType(null);
        }
        if (addressRegistryBaseType.getStreet() != null && addressRegistryBaseType.getStreet().getType() != null
                && addressRegistryBaseType.getStreet().getType().getFull() == null) {
            addressRegistryBaseType.getStreet().setType(null);
        }
        if (addressRegistryBaseType.getBuilding() != null) {
            if (addressRegistryBaseType.getBuilding().getConstruction() != null
                    && addressRegistryBaseType.getBuilding().getConstruction().getType() != null
                    && addressRegistryBaseType.getBuilding().getConstruction().getType().getFull() == null) {
                addressRegistryBaseType.getBuilding().getConstruction().setType(null);
            }
            if (addressRegistryBaseType.getBuilding().getBuild() != null
                    && addressRegistryBaseType.getBuilding().getBuild().getType() != null
                    && addressRegistryBaseType.getBuilding().getBuild().getType().getFull() == null) {
                addressRegistryBaseType.getBuilding().getBuild().setType(null);
            }
            if (addressRegistryBaseType.getBuilding().getHouse() != null
                    && addressRegistryBaseType.getBuilding().getHouse().getType() != null
                    && addressRegistryBaseType.getBuilding().getHouse().getType().getFull() == null) {
                addressRegistryBaseType.getBuilding().getHouse().setType(null);
            }
        }
        return addressRegistryBaseType;
    }
}
