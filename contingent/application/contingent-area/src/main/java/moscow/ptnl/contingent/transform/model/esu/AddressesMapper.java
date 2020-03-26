package moscow.ptnl.contingent.transform.model.esu;

import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.model.area.AddressLevelType;
import moscow.ptnl.contingent.transform.Transform;
import moscow.ptnl.contingent2.area.info.Address;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.Set;

@Component
public class AddressesMapper implements Transform<AreaInfoEvent.Addresses, Set<AreaAddress>> {

    @Override
    public AreaInfoEvent.Addresses entityToDtoTransform(Set<AreaAddress> entity) {
        AreaInfoEvent.Addresses addresses = new AreaInfoEvent.Addresses();
        entity.stream()
                .map(AreaAddress::getAddress)
                .filter(e -> AddressLevelType.REGION_TE.getLevel().equals(e.getAoLevel())
                        || AddressLevelType.ID.getLevel().equals(e.getAoLevel())
                        || AddressLevelType.AREA_TE.getLevel().equals(e.getAoLevel())
                        || AddressLevelType.AREA.getLevel().equals(e.getAoLevel())
                            && e.getAreaBtiCode() != null
                        || AddressLevelType.CITY.getLevel().equals(e.getAoLevel())
                            && e.getCityBtiCode() != null
                        || AddressLevelType.PLACE.getLevel().equals(e.getAoLevel())
                            && e.getPlaceBtiCode() != null
                        || AddressLevelType.STREET.getLevel().equals(e.getAoLevel())
                            && (e.getStreetBtiCode() != null || e.getStreetOmkUm() != null)
                        || AddressLevelType.PLAN.getLevel().equals(e.getAoLevel())
                            && e.getPlanBtiCode() != null
                )
                .forEach(e -> {
                    Address address = new Address();
                    // Уровень адреса (aolevel) = AOLEVEL (заполняется для всех уровней адреса);
                    address.setAolevel(e.getAoLevel());
                    // Код округа (omkTeDistrictCode) = REGION_TE_CODE (заполняется для всех уровней адреса);
                    address.setOmkTeDistrictCode(e.getRegionTeCode());

                    Address.StreetBTI streetBTI = new Address.StreetBTI();

                    // Код района (omkTeRegionCode) = AREACODE_OMK_TE (заполняется для всех уровней адреса, кроме 2);
                    if (!AddressLevelType.REGION_TE.getLevel().equals(e.getAoLevel())) {
                        address.setOmkTeRegionCode(e.getAreaCodeOmkTe());
                    }

                    // Код улицы по ОМК УМ (omkUmCode) = STREET_OMK_UM (может быть заполнен для уровней адреса 7, 8);
                    if (AddressLevelType.STREET.getLevel().equals(e.getAoLevel()) ||
                            AddressLevelType.ID.getLevel().equals(e.getAoLevel())) {
                        address.setOmkUmCode(e.getStreetOmkUm());
                    }

                    // - 3 - AREA_BTI_CODE,
                    if (AddressLevelType.AREA.getLevel().equals(e.getAoLevel())) {
                        streetBTI.getCode().addAll(Arrays.asList(e.getAreaBtiCode().split(";")));
                    }

                    // - 4 - CITY_BTI_CODE,
                    if (AddressLevelType.CITY.getLevel().equals(e.getAoLevel())) {
                        streetBTI.getCode().addAll(Arrays.asList(e.getCityBtiCode().split(";")));
                    }

                    // - 6 - PLACE_BTI_CODE,
                    if (AddressLevelType.PLACE.getLevel().equals(e.getAoLevel())) {
                        streetBTI.getCode().addAll(Arrays.asList(e.getPlaceBtiCode().split(";")));
                    }

                    // - 6 - PLACE_BTI_CODE,
                    if (AddressLevelType.PLAN.getLevel().equals(e.getAoLevel())) {
                        streetBTI.getCode().addAll(Arrays.asList(e.getPlanBtiCode().split(";")));
                    }

                    // - 7 - STREET_BTI_CODE,
                    if (AddressLevelType.STREET.getLevel().equals(e.getAoLevel()) && e.getStreetBtiCode() != null) {
                        streetBTI.getCode().addAll(Arrays.asList(e.getStreetBtiCode().split(";")));
                    }

                    if (AddressLevelType.ID.getLevel().equals(e.getAoLevel())) {
                        address.setHouse(e.getL1Value());
                        address.setBuilding(e.getL2Value());
                        address.setConstruction(e.getL3Value());

                        if (e.getStreetBtiCode() != null) {
                            streetBTI.getCode().addAll(Arrays.asList(e.getStreetBtiCode().split(";")));
                        }else if (e.getPlanBtiCode() != null) {
                            streetBTI.getCode().addAll(Arrays.asList(e.getPlanBtiCode().split(";")));
                        }else if (e.getPlaceBtiCode() != null) {
                            streetBTI.getCode().addAll(Arrays.asList(e.getPlaceBtiCode().split(";")));
                        }else if (e.getCityBtiCode() != null) {
                            streetBTI.getCode().addAll(Arrays.asList(e.getCityBtiCode().split(";")));
                        }else if (e.getAreaBtiCode() != null) {
                            streetBTI.getCode().addAll(Arrays.asList(e.getAreaBtiCode().split(";")));
                        }
                    }

                    address.setStreetBTI(streetBTI);
                    addresses.getAddress().add(address);
                });
        return addresses;
    }

    @Override
    public Set<AreaAddress> dtoToEntityTransform(AreaInfoEvent.Addresses dtoObject) {
        return null;
    }
}
