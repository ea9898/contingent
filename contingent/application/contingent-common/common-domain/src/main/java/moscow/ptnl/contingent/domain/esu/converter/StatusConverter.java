package moscow.ptnl.contingent.domain.esu.converter;

import jakarta.persistence.AttributeConverter;
import moscow.ptnl.contingent.domain.esu.EsuStatusType;

/**
 *
 * @author mkachalov
 */
public class StatusConverter implements AttributeConverter<EsuStatusType, Integer> {

    @Override
    public Integer convertToDatabaseColumn(EsuStatusType attribute) {
        if (attribute == null) {
            return null;
        }
        return attribute.getValue();
    }

    @Override
    public EsuStatusType convertToEntityAttribute(Integer dbData) {
        if (dbData == null) {
            return null;
        }
        return EsuStatusType.getByValue(dbData);
    }
    
}
