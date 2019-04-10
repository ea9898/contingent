package moscow.ptnl.contingent.area.entity.converter;

import javax.persistence.AttributeConverter;
import javax.persistence.Converter;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author mkomlev
 * Конвертер Integer в строгий флаг (not null)
 */
@Converter
public class BooleanStrictIntegerConverter implements AttributeConverter<Boolean, Integer>, Serializable {

	private static final long serialVersionUID = 7154235777093175513L;

	@Override
    public Integer convertToDatabaseColumn(Boolean value) {
        return Boolean.TRUE.equals(value) ? 1 : 0;
    }

    @Override
    public Boolean convertToEntityAttribute(Integer value) {
        return Objects.equals(1, value);
    }

}
