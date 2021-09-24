package moscow.ptnl.contingent.area.transform.v2;

import moscow.ptnl.contingent.area.transform.Transformer;
import org.springframework.stereotype.Component;

@Component
public class LongToStringTransformer implements Transformer {

    @Override
    public Object transform(Object source, Class<?> targetClass) {
        return source != null ? ((Long) source).toString() : null;
    }

    @Override
    public boolean suitable(Class<?> sourceClass, Class<?> targetClass) {
        return Long.class.isAssignableFrom(sourceClass)
                && String.class.isAssignableFrom(targetClass);
    }

    @Override
    public int getClassOrder() {
        return 0;
    }
}
