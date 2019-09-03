package moscow.ptnl.contingent.nsi.pushaccepter;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClass;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeKind;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeRelations;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeSpecializations;
import moscow.ptnl.contingent.area.entity.nsi.Gender;
import moscow.ptnl.contingent.area.entity.nsi.PositionCode;
import moscow.ptnl.contingent.area.entity.nsi.Specialization;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing.Package;
import ru.mos.emias.nsiproduct.core.v1.EhdCatalogRow;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsiHelper;

public class NsiEntityMapper {

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

    public static List<AreaType> mapAreaTypes(List<EhdCatalogRow> rows) {
        return rows.stream().map(NsiEntityMapper::mapAreaType).collect(Collectors.toList());
    }

    public static List<AreaTypeClass> mapAreaTypeClasses(List<EhdCatalogRow> rows) {
        return rows.stream().map(NsiEntityMapper::mapAreaTypeClass).collect(Collectors.toList());
    }

    public static List<AreaTypeKind> mapAreaTypeKinds(List<EhdCatalogRow> rows) {
        return rows.stream().map(NsiEntityMapper::mapAreaTypeKind).collect(Collectors.toList());
    }

    public static List<AreaTypeMedicalPositions> mapAreaTypeMedicalPositions(List<EhdCatalogRow> rows) {
        return rows.stream().map(NsiEntityMapper::mapAreaTypeMedicalPosition).collect(Collectors.toList());
    }

    public static List<AreaTypeRelations> mapAreaTypeRelations(List<EhdCatalogRow> rows) {
        return rows.stream().map(NsiEntityMapper::mapAreaTypeRelation).collect(Collectors.toList());
    }

    public static List<AreaTypeSpecializations> mapAreaTypeSpecializations(List<EhdCatalogRow> rows) {
        return rows.stream().map(NsiEntityMapper::mapAreaTypeSpecialization).collect(Collectors.toList());
    }

    public static List<Specialization> mapSpecializations(List<EhdCatalogRow> rows) {
        return rows.stream().map(NsiEntityMapper::mapSpecialization).collect(Collectors.toList());
    }

    public static List<PositionCode> mapPositionCodes(List<EhdCatalogRow> rows) {
        return rows.stream().map(NsiEntityMapper::mapPositionCode).collect(Collectors.toList());
    }

    public static List<Gender> mapGenders(List<EhdCatalogRow> rows) {
        return rows.stream().map(NsiEntityMapper::mapGender).collect(Collectors.toList());
    }

    public static AreaType mapAreaType(EhdCatalogRow row) {
        return mapEhdCatalogRow(row, AreaType.class);
    }

    public static AreaType mapAreaType(Package row) {
        return mapEhdCatalogRow(row, AreaType.class);
    }

    public static AreaTypeClass mapAreaTypeClass(EhdCatalogRow row) {
        return mapEhdCatalogRow(row, AreaTypeClass.class);
    }

    public static AreaTypeClass mapAreaTypeClass(Package row) {
        return mapEhdCatalogRow(row, AreaTypeClass.class);
    }

    public static AreaTypeKind mapAreaTypeKind(EhdCatalogRow row) {
        return mapEhdCatalogRow(row, AreaTypeKind.class);
    }

    public static AreaTypeKind mapAreaTypeKind(Package row) {
        return mapEhdCatalogRow(row, AreaTypeKind.class);
    }

    public static AreaTypeMedicalPositions mapAreaTypeMedicalPosition(EhdCatalogRow row) {
        return mapEhdCatalogRow(row, AreaTypeMedicalPositions.class);
    }

    public static AreaTypeMedicalPositions mapAreaTypeMedicalPosition(Package row) {
        return mapEhdCatalogRow(row, AreaTypeMedicalPositions.class);
    }

    public static AreaTypeRelations mapAreaTypeRelation(EhdCatalogRow row) {
        return mapEhdCatalogRow(row, AreaTypeRelations.class);
    }

    public static AreaTypeRelations mapAreaTypeRelation(Package row) {
        return mapEhdCatalogRow(row, AreaTypeRelations.class);
    }

    public static AreaTypeSpecializations mapAreaTypeSpecialization(EhdCatalogRow row) {
        return mapEhdCatalogRow(row, AreaTypeSpecializations.class);
    }

    public static AreaTypeSpecializations mapAreaTypeSpecialization(Package row) {
        return mapEhdCatalogRow(row, AreaTypeSpecializations.class);
    }

    public static Specialization mapSpecialization(EhdCatalogRow row) {
        return mapEhdCatalogRow(row, Specialization.class);
    }

    public static Specialization mapSpecialization(Package row) {
        return mapEhdCatalogRow(row, Specialization.class);
    }

    public static PositionCode mapPositionCode(EhdCatalogRow row) {
        return mapEhdCatalogRow(row, PositionCode.class);
    }

    public static PositionCode mapPositionCode(Package row) {
        return mapEhdCatalogRow(row, PositionCode.class);
    }

    public static Gender mapGender(EhdCatalogRow row) {
        return mapEhdCatalogRow(row, Gender.class);
    }

    public static Gender mapGender(Package row) {
        return mapEhdCatalogRow(row, Gender.class);
    }
    
    public static <T> T mapEhdCatalogRow(Object row, Class<T> clazz) {
        try {
            T target = clazz.newInstance();

            Optional<NsiTablesEnum> defaultTable = MapToNsiHelper.getNsiTableFromClass(clazz);

            MapToNsiHelper.getNsiAnnotatedFields(clazz).forEach((field, annotation) -> {
                NsiTablesEnum table = !NsiTablesEnum.UNKNOWN.equals(annotation.table()) 
                        ? annotation.table() 
                        : defaultTable.orElse(NsiTablesEnum.UNKNOWN);
                if (NsiTablesEnum.UNKNOWN.equals(table)) {
                    throw new IllegalStateException("не удалось определить имя таблицы НСИ для поля: [" + field.getName() + "]");
                }

                String parameterName = MapToNsiHelper.getNsiFieldName(field, annotation);            
                Integer tableCode = table.getCode();

                Object value = getValue(row, parameterName, tableCode);

                MapToNsiHelper.setFieldValue(field, target, value, annotation);
            });

            return target;
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }
}
