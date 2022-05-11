package moscow.ptnl.contingent.area.transform.v3;

import moscow.ptnl.contingent.area.transform.OptionEnum;
import moscow.ptnl.contingent.area.transform.SortingFieldEnum;
import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.core.v3.KeyValuePair;
import ru.mos.emias.contingent2.core.v3.Options;
import ru.mos.emias.contingent2.core.v3.PagingOptions;
import ru.mos.emias.contingent2.core.v3.PagingResults;
import ru.mos.emias.contingent2.core.v3.SortOrder;
import ru.mos.emias.contingent2.core.v3.SortingOptions;

import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

@Component
public class SoapCustomMapperV3 {

    @Autowired
    private SettingService settingService;

    public void mapPagingResults(PagingResults results, Page<?> page) {
        results.setPageNumber(page.getNumber());
        results.setPageSize(page.getSize());
        results.setPageTotal(page.getTotalPages());
        results.setMorePagesAvailable(page.getNumber() < page.getTotalPages() - 1);
        results.setTotalItemsCount(page.getTotalElements());
    }

    public <T extends OptionEnum> Map<T, OptionEnum.OptionValuesEnum> mapOptions(
            Options options, Class<T> optionsClass) throws ContingentException {
        Map<T, OptionEnum.OptionValuesEnum> result = new HashMap<>();

        if (options == null) {
            return result;
        }
        Validation validation = new Validation();
        List<T> keys = Arrays.asList(optionsClass.getEnumConstants());

        for (KeyValuePair option : options.getEntries()) {
            T key = keys.stream()
                    .filter(k -> Objects.equals(k.getKeyName(), option.getKey()))
                    .findFirst()
                    .orElse(null);
            if (key == null) {
                validation.error(AreaErrorReason.INCORRECT_OPTIONS_KEY,
                        new ValidationParameter("key", keys.stream().map(OptionEnum::getKeyName).collect(Collectors.joining(", "))));
            } else {
                OptionEnum.OptionValuesEnum value = key.getPossibleValues().stream()
                        .filter(k -> Objects.equals(k.name(), option.getValue()))
                        .findFirst()
                        .orElse(null);
                if (value == null) {
                    validation.error(AreaErrorReason.INCORRECT_OPTIONS_VALUE,
                            new ValidationParameter("value", key.getPossibleValues().stream().map(Enum::name).collect(Collectors.joining(", "))));
                }
                if (result.containsKey(key)) {
                    validation.error(AreaErrorReason.OPTIONS_KEY_IS_NOT_UNIQUE, new ValidationParameter("key", key.getKeyName()));
                }
                result.put(key, value);
            }
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        return result;
    }

    public Sort mapSorting(SortingOptions sorting, EnumSet<? extends SortingFieldEnum> fields) throws ContingentException {
        Sort sort = Sort.unsorted();

        if (sorting == null || fields == null) {
            return sort;
        }
        for (SortOrder sortOrder : sorting.getSortOrders()) {
            SortingFieldEnum field = fields.stream()
                    .filter(f -> Objects.equals(sortOrder.getAttributeName(), f.toString()))
                    .findFirst()
                    .orElse(null);
            if (field == null) {
                throw new ContingentException(new Validation().error(AreaErrorReason.INCORRECT_SORTING_FIELD,
                        new ValidationParameter("attributeName", fields.stream().map(Enum::toString).collect(Collectors.joining(", ")))));
            }
            sort = sort.and(Sort.by(Boolean.TRUE.equals(sortOrder.isDesceding()) ? Sort.Direction.DESC : Sort.Direction.ASC, field.getFieldName()));
        }
        return sort;
    }

    public PageRequest mapPagingOptions(PagingOptions options, EnumSet<? extends SortingFieldEnum> fields) throws ContingentException {
        if (options == null) {
            return PageRequest.of(settingService.getPar5(), settingService.getPar6());
        }
        Sort sort = mapSorting(options.getSortingOptions(), fields);

        if (options.getPageSize() > settingService.getPar3()) {
            throw new ContingentException(AreaErrorReason.TOO_BIG_PAGE_SIZE, new ValidationParameter("pageSize", settingService.getPar3()));
        }

        if (options.getPageNumber() < 0 || options.getPageSize() <= 0) {
            throw new ContingentException(AreaErrorReason.PAGING_INCORRECT);
        }

        return PageRequest.of(options.getPageNumber(), options.getPageSize(), sort);
    }
}
