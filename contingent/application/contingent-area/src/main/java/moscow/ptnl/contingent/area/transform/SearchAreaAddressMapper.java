package moscow.ptnl.contingent.area.transform;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import ru.mos.emias.contingent2.address.SearchAreaAddress;

@Mapper(componentModel="spring")
public interface SearchAreaAddressMapper {

    @Mappings({
            @Mapping(target="aoLevel", source="aoLevel"),
            @Mapping(target="globalIdNsi", source="globalIdNsi"),
            @Mapping(target="regionOMKTEcode", source="regionOMKTEcode"),
            @Mapping(target="areaOMKTEcode", source="areaOMKTEcode"),
            @Mapping(target="areaCode", source="areaCode"),
            @Mapping(target="cityCode", source="cityCode"),
            @Mapping(target="placeCode", source="placeCode"),
            @Mapping(target="planCode", source="planCode"),
            @Mapping(target="streetCode", source="streetCode"),
            @Mapping(target="house", source="house"),
            @Mapping(target="build", source="build"),
            @Mapping(target="construction", source="construction")
    })
    SearchAreaAddress entityToDtoTransform(moscow.ptnl.contingent.domain.area.model.area.SearchAreaAddress entityObject);

    @Mappings({
            @Mapping(target="aoLevel", source="aoLevel"),
            @Mapping(target="globalIdNsi", source="globalIdNsi"),
            @Mapping(target="regionOMKTEcode", source="regionOMKTEcode"),
            @Mapping(target="areaOMKTEcode", source="areaOMKTEcode"),
            @Mapping(target="areaCode", source="areaCode"),
            @Mapping(target="cityCode", source="cityCode"),
            @Mapping(target="placeCode", source="placeCode"),
            @Mapping(target="planCode", source="planCode"),
            @Mapping(target="streetCode", source="streetCode"),
            @Mapping(target="house", source="house"),
            @Mapping(target="build", source="build"),
            @Mapping(target="construction", source="construction")
    })
    moscow.ptnl.contingent.domain.area.model.area.SearchAreaAddress dtoToEntityTransform(SearchAreaAddress dtoObject);

}
