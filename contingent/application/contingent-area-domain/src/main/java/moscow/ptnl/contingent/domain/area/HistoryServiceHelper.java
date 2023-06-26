package moscow.ptnl.contingent.domain.area;

import java.util.Objects;

public interface HistoryServiceHelper {

    <T> T clone(T object);

    <T> void sendHistory(T oldObject, T newObject, Class<T> cls, Long eventTypeId, Long operationLinkId);

    default <T> void sendHistory(T oldObject, T newObject, Class<T> cls, Long eventTypeId) {
        sendHistory(oldObject, newObject, cls, eventTypeId, null);
    }

    default <T> void sendHistory(T oldObject, T newObject, Class<T> cls) {
        sendHistory(oldObject, newObject, cls, null, null);
    }

    default Long getEventTypeByEmployeeCategory(Long medicalEmployeeCategory) {
        if (Objects.equals(medicalEmployeeCategory, 0L)) {
            return 4L;
        } else if (Objects.equals(medicalEmployeeCategory, 1L)) {
            return 9L;
        } else if (Objects.equals(medicalEmployeeCategory, 2L)) {
            return 7L;
        }
        return null;
    }
}
