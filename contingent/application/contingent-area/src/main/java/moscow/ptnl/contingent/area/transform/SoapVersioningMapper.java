package moscow.ptnl.contingent.area.transform;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.xml.bind.annotation.XmlType;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author sorlov
 */
@Component
public class SoapVersioningMapper {

    @Autowired
    private List<Transformer> customTransformers;

    public <T1, T2> T2 map(T1 source, T2 target) throws NoSuchFieldException, IllegalAccessException, InstantiationException {
        Map<String, Field> targetFields = getAllClassFields(target.getClass()).stream()
                .collect(Collectors.toMap(Field::getName, f -> f));
        //Обходим поля исходного объекта
        for (Field field : getAllClassFields(source.getClass())) {
            if (Modifier.isFinal(field.getModifiers()) || Modifier.isStatic(field.getModifiers()) || Modifier.isTransient(field.getModifiers())) {
                continue;
            }
            field.setAccessible(true);
            Field targetField = targetFields.get(field.getName());

            if (targetField == null) {
                continue;
            }
            targetField.setAccessible(true);
            Object value = field.get(source);
            Transformer transformer;

            if (value == null) {
                targetField.set(target, null);
            }
            else if (Collection.class.isAssignableFrom(field.getType())) {
                targetField.set(target, copyCollection((Collection<?>) value, (Collection<?>) value.getClass().newInstance(), targetField.getGenericType()));
            }
            else if ((transformer = customTransformers.stream()
                    .filter(t -> t.suitable(field.getType(), targetField.getType()))
                    .max(Comparator.comparing(Transformer::getClassOrder))
                    .orElse(null)) != null) {
                targetField.set(target, transformer.transform(value, targetField.getType()));
            }
            else if (field.getType().getAnnotation(XmlType.class) != null) {
                targetField.set(target, map(value, targetField.getType().newInstance()));
            }
            else {
                targetField.set(target, value);
            }
        }
        return target;
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private Collection copyCollection(Collection source, Collection target, Type targetValueType) throws IllegalAccessException, InstantiationException, NoSuchFieldException {
        ParameterizedType paramType = (ParameterizedType) targetValueType;
        Class<?> targetClass = (Class<?>) paramType.getActualTypeArguments()[0];

        for (Object obj : source) {
            Object clone = obj == null ? null :
                    (targetClass.getAnnotation(XmlType.class) == null ? obj : map(obj, targetClass.newInstance()));
            target.add(clone);
        }
        return target;
    }

    private static List<Field> getAllClassFields(Class<?> sourceClass) {
        List<Field> currentClassFields = new LinkedList<>(Arrays.asList(sourceClass.getDeclaredFields()));
        Class<?> parentClass = sourceClass.getSuperclass();

        if (parentClass != null && !(parentClass.equals(Object.class))) {
            List<Field> parentClassFields = getAllClassFields(parentClass);
            currentClassFields.addAll(parentClassFields);
        }
        return currentClassFields;
    }
}
