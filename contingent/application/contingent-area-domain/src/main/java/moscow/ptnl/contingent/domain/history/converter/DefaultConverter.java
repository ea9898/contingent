package moscow.ptnl.contingent.domain.history.converter;

import moscow.ptnl.contingent.domain.history.EntityConverterHelper;
import moscow.ptnl.contingent.domain.history.meta.FieldConverter;

/**
 * Конвертер для работы с популярными типами данных.
 * 
 * @author m.kachalov
 */
public class DefaultConverter implements FieldConverter {
    
    @Override
    public boolean equals(Object value1, Object value2) {        
        return EntityConverterHelper.equals(value1, value2);
    }

    @Override
    public String toString(Object value) {
        return EntityConverterHelper.toString(value);
    }
}
