package moscow.ptnl.contingent.domain.history.converter;

import moscow.ptnl.contingent.domain.history.EntityConverterHelper;
import moscow.ptnl.contingent.domain.history.meta.FieldConverter;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;

import java.util.Objects;

public class AreaTypeFieldConverter implements FieldConverter {

    @Override
    public boolean equals(Object value1, Object value2) {
        return Objects.equals(value1, value2);
    }

    @Override
    public String toString(Object value) {
        return value instanceof AreaType ? EntityConverterHelper.toString(((AreaType) value).getCode()) : null;
    }
}
