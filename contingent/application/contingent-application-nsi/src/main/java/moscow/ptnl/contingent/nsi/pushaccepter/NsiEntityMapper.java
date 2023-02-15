package moscow.ptnl.contingent.nsi.pushaccepter;

import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing.Package;
import ru.mos.emias.nsiproduct.core.v1.EhdCatalogRow;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import moscow.ptnl.contingent.nsi.domain.helper.MapToNsiHelper;
import moscow.ptnl.contingent.nsi.domain.helper.NsiMapperUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class NsiEntityMapper {
    
    private final static Logger LOG = LoggerFactory.getLogger(NsiEntityMapper.class);
    
    @Autowired
    private MapToNsiHelper mapToNsiHelper;

    private static Object getValue(Object row, String parameterName, Integer tableCode) {
        String errorMessage = String.format("Нет такого поля %s в таблице %s", parameterName, tableCode);
        if (row instanceof EhdCatalogRow) {
            return ((EhdCatalogRow) row).getItem().stream().filter(atr -> atr.getTehName().equalsIgnoreCase(parameterName))
                    .findFirst().orElseThrow(() -> new IllegalStateException(errorMessage)).getValue();
        } else if (row instanceof Package) {
            return ((Package) row).catalog.data.attribute.stream().filter(atr -> atr.name.equalsIgnoreCase(parameterName))
                    .findFirst().orElseThrow(() -> new IllegalStateException(errorMessage)).values.value.value;
        }
        return null;
    }
    
    public <T> List<T> mapTypedList(List<EhdCatalogRow> rows, Class<T> type) {
        return rows.stream().map(r -> mapTyped(r, type)).collect(Collectors.toList());
    }
    
    public  <T> T mapTyped(EhdCatalogRow row, Class<T> type) {
        return mapEhdCatalogRow(row, type);
    }
    
    public  <T> T mapTyped(Package row, Class<T> type) {
        return mapEhdCatalogRow(row, type);
    }
    
    public <T> T mapEhdCatalogRow(Object row, Class<T> clazz) {
        try {
            T target = clazz.newInstance();

            Optional<NsiTablesEnum> defaultTable = NsiMapperUtil.getNsiTableFromClass(clazz);

            NsiMapperUtil.getNsiAnnotatedFields(clazz).forEach((field, annotation) -> {
                NsiTablesEnum table = !NsiTablesEnum.UNKNOWN.equals(annotation.table()) 
                        ? annotation.table() 
                        : defaultTable.orElse(NsiTablesEnum.UNKNOWN);
                if (NsiTablesEnum.UNKNOWN.equals(table)) {
                    throw new IllegalStateException("не удалось определить имя таблицы НСИ для поля: [" + field.getName() + "]");
                }

                String parameterName = NsiMapperUtil.getNsiFieldName(field, annotation);            
                Integer tableCode = table.getCode();

                Object value = getValue(row, parameterName, tableCode);

                mapToNsiHelper.setFieldValue(field, target, value, annotation);
            });

            return target;
        } catch (RuntimeException e) {
            LOG.error("ошибка маппинга строки", e);
            throw e;
        } catch (Exception e) {
            LOG.error("ошибка маппинга строки", e);
            throw new IllegalStateException(e);
        }
    }
}
