package moscow.ptnl.contingent.area.transform.v2;

import moscow.ptnl.contingent.area.transform.Transformer;
import org.springframework.stereotype.Component;

@Component
public class StringToLongTransformer implements Transformer {

    @Override
    public Object transform(Object source, Class<?> targetClass) {
        return Long.parseLong((String) source);
    }

    @Override
    public boolean suitable(Class<?> sourceClass, Class<?> targetClass) {
        return String.class.isAssignableFrom(sourceClass)
                && Long.class.isAssignableFrom(targetClass);
    }

    @Override
    public int getClassOrder() {
        return 0;
    }
}
