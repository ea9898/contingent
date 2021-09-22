package moscow.ptnl.contingent.area.transform;

public interface Transformer {

    Object transform(Object source, Class<?> targetClass);

    boolean suitable(Class<?> sourceClass, Class<?> targetClass);

    int getClassOrder();
}
