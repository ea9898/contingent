package moscow.ptnl.contingent.domain.esu.converter;

import javax.persistence.AttributeConverter;
import moscow.ptnl.contingent.domain.esu.EsuOutput;

/**
 *
 * @author mkachalov
 */
public class StatusConverter implements AttributeConverter<EsuOutput.STATUS, Integer> {

    @Override
    public Integer convertToDatabaseColumn(EsuOutput.STATUS attribute) {
        if (attribute == null) {
            return null;
        }
        return attribute.getValue();
    }

    @Override
    public EsuOutput.STATUS convertToEntityAttribute(Integer dbData) {
        if (dbData == null) {
            return null;
        }
        return EsuOutput.STATUS.getByValue(dbData);
    }
    
}
