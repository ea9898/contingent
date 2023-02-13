package moscow.ptnl.contingent.domain.converter;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import moscow.ptnl.contingent.domain.settings.SettingValueType;

/**
 *
 * @author m.kachalov
 */
@Converter(autoApply = false)
public class SettingValueTypeConverter implements AttributeConverter<SettingValueType, Long> {

    @Override
    public Long convertToDatabaseColumn(SettingValueType attribute) {
        if (attribute == null) {
            return null;
        }
        return attribute.getTypeCode();
    }

    @Override
    public SettingValueType convertToEntityAttribute(Long dbData) {
        if (dbData == null) {
            return null;
        }
        return SettingValueType.valueOf(dbData);
    }
    
}
