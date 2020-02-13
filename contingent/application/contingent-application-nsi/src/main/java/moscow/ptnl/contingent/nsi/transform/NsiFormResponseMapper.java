package moscow.ptnl.contingent.nsi.transform;

import moscow.ptnl.util.XMLUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.persistence.Column;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

@Component
public class NsiFormResponseMapper {

    private final static Logger LOG = LoggerFactory.getLogger(NsiFormResponseMapper.class);

    private final static Map<String, String> FIELDS_MAPPING;

    static {
        FIELDS_MAPPING = new HashMap<>();
        FIELDS_MAPPING.put("REGION_TE_CODE", "REGION_TE_TE_CODE");
        FIELDS_MAPPING.put("REGION_CODE", "REGIONCODE");
        FIELDS_MAPPING.put("AREACODE_OMK_TE", "AREA_TE_TE_CODE");
    }

    public void transformAndMergeEntity(Document document, Object entityObj) throws IllegalAccessException {
        Class<?> entityClass = entityObj.getClass();
        List<String> unknownFields = new ArrayList<>();
        Node root = document.getElementsByTagName("response").item(0);
        Integer count = Integer.valueOf(root.getAttributes().getNamedItem("count").getNodeValue());
        Node entity = root.getChildNodes().item(0);

        for (int i = 0; i < entity.getChildNodes().getLength(); i++) {
            Node valueNode = entity.getChildNodes().item(i);

            if ("multifield".equals(valueNode.getNodeName())) {
                mapMultiField(valueNode, entityObj, unknownFields);
            }
            else {
                Field field = getEntityField(entityObj.getClass(), valueNode.getNodeName());

                if (field != null) {
                    Object value = mapSimpleField(valueNode);
                    field.setAccessible(true);
                    field.set(entityObj, cast(value, field.getType()));
                }
                else {
                    unknownFields.add(valueNode.getNodeName());
                }
            }
        }
        if (!unknownFields.isEmpty()) {
            LOG.debug("Не найдены поля {} сущности {}", String.join( ", ", unknownFields), entityClass.getSimpleName());
//            throw new RuntimeException("Не найдены поля сущности " + String.join( ", ", unknownFields));
        }
    }

    private void mapMultiField(Node valueNode, Object entityObj, List<String> unknownFields) throws IllegalAccessException {
        NodeList values = ((Element) valueNode).getElementsByTagName("field");
        Map<String, List<String>> fields = XMLUtil.asList(values).stream()
                .flatMap(v -> XMLUtil.asList(v.getChildNodes()).stream())
                .collect(Collectors.groupingBy(Node::getNodeName, Collectors.mapping(this::mapSimpleField, Collectors.toList())));

        for (Map.Entry<String, List<String>> entry : fields.entrySet()) {
            Field field = getEntityField(entityObj.getClass(), entry.getKey());

            if (field != null) {
                field.setAccessible(true);
                field.set(entityObj, cast(String.join(";", entry.getValue()), field.getType()));
            }
            else {
                unknownFields.add(entry.getKey());
            }
        }
    }

    private String mapSimpleField(Node valueNode) {
        NodeList values = ((Element) valueNode).getElementsByTagName("value");
        return values.getLength() > 0 ?
                XMLUtil.asList(values).stream()
                        .map(Node::getTextContent)
                        .map(NsiFormResponseMapper::valueToString)
                        .distinct()
                        .collect(Collectors.joining(";"))
                : valueNode.getTextContent();
    }

    private Field getEntityField(Class<?> entityClass, String fieldName) {
        Field[] fields = entityClass.getDeclaredFields();

        for (Field field : fields) {
            Column ann = field.getAnnotation(Column.class);
            String mappedName = ann != null ? FIELDS_MAPPING.get(ann.name()) : null;

            if (ann != null
                    && ((Objects.equals(ann.name(), fieldName)) && mappedName == null
                    || (Objects.equals(mappedName, fieldName)))) {
                return field;
            }
        }
        return null;
    }

    private <T> T cast(Object value, Class<T> fieldType) {
        //если поле простого типа
        switch (fieldType.getSimpleName()) {
            case "String":
                return fieldType.cast(valueToString(value));
            case "Long":
                return fieldType.cast(valueToLong(value));
            case "Integer":
                return fieldType.cast(valueToInteger(value));
            case "Boolean":
                return fieldType.cast(valueToBoolean(value));
            case "LocalDate":
                return fieldType.cast(valueToLocalDate(value));
            default:
                throw new IllegalArgumentException("не поддерживаемый тип поля для сущности: [" + fieldType.getSimpleName() +"]");
        }
    }

    private static String valueToString(Object value) {
        if (value == null) {
            return null;
        }
        String result = value.toString();
        return (result.isEmpty()) ? null : result;
    }

    private static Long valueToLong(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof BigDecimal) {
            return ((BigDecimal) value).longValue();
        } else if (value instanceof String) {
            return Long.parseLong((String) value);
        }
        throw new IllegalArgumentException("не поддерживаемый тип: [" + value.getClass().getName() + "]");
    }

    private static Integer valueToInteger(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof BigDecimal) {
            return ((BigDecimal) value).intValue();
        } else if (value instanceof String) {
            return Integer.parseInt((String) value);
        }
        throw new IllegalArgumentException("не поддерживаемый тип: [" + value.getClass().getName() + "]");
    }

    private static Boolean valueToBoolean(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof String) {
            String result = (String) value;
            return (result.isEmpty()) ? null : "1".equals(result);
        }
        throw new IllegalArgumentException("не поддерживаемый тип: [" + value.getClass().getName() + "]");
    }

    private static LocalDate valueToLocalDate(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof String) {
            String result = (String) value;
            return LocalDateTime.parse(result, DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss")).toLocalDate();
        }
        throw new IllegalArgumentException("не поддерживаемый тип: [" + value.getClass().getName() + "]");
    }
}
