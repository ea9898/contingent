package moscow.ptnl.contingent.domain.area;

public interface HistoryServiceHelper {

    <T> T clone(T object);

    <T> void sendHistory(T oldObject, T newObject, Class<T> cls);
}
