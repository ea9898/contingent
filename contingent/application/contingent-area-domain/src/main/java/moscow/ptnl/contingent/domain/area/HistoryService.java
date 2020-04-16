package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.security.Principal;


/**
 * Сервис журналирования объектов.
 * 
 * @author m.kachalov
 */
public interface HistoryService {
    
    /**
     * Клонирование объекта.
     * Не глубокое - копируются все поля сущности кроме аннотированных как 
     * Transient.
     * 
     * @param <T>
     * @param object
     * @return 
     * @throws RuntimeException
     */
    <T> T clone(T object) throws RuntimeException;
    
    /**
     * Запись изменных полей сущности в журнал логирования изменений.
     * 
     * @param <T>
     * @param requestUuid заполняется как UserContextHolder.getRequestUUID()
     * @param methodName имя метода web-сервиса при вызове которого происходит запись в историю: UserContextHolder.getMethodName()
     * @param principal заполняется как UserContextHolder.getPrincipal()
     * @param oldObject
     * @param newObject
     * @throws RuntimeException 
     */
    <T> void write(String requestUuid, String methodName, Principal principal, T oldObject, T newObject, Class<T> cls) throws RuntimeException;
        
}
