package moscow.ptnl.contingent.domain.trigger.converter;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import moscow.ptnl.contingent.domain.trigger.TriggerName;

/**
 *
 * @author m.kachalov
 */
@Converter(autoApply = true)
public class TriggerConverter implements AttributeConverter<TriggerName, String> {

    @Override
    public String convertToDatabaseColumn(TriggerName attribute) {
        return (attribute != null) ? attribute.getName() : null;
    }

    @Override
    public TriggerName convertToEntityAttribute(String dbData) {
        return TriggerName.getByName(dbData).orElse(null);
    }
    
}
