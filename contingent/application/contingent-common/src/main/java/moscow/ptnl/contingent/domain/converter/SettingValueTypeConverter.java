/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.domain.converter;

import javax.persistence.AttributeConverter;
import javax.persistence.Converter;
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
