package moscow.ptnl.contingent.service.history;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import javax.persistence.Transient;
import moscow.ptnl.contingent.configuration.EventChannelsConfiguration;
import moscow.ptnl.contingent.domain.history.EntityConverterHelper;
import moscow.ptnl.contingent.domain.history.HistoryEventBuilder;
import moscow.ptnl.contingent.domain.history.ServiceName;
import moscow.ptnl.contingent.domain.history.converter.DefaultConverter;
import moscow.ptnl.contingent.domain.history.meta.FieldConverter;
import moscow.ptnl.contingent.domain.history.meta.Journalable;
import moscow.ptnl.contingent.domain.history.meta.LogIt;
import moscow.ptnl.contingent.security.Principal;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Service;

/**
 *
 * @author m.kachalov
 */
@Service
public class HistoryServiceImpl implements HistoryService {
    
    private static final Logger LOG = LoggerFactory.getLogger(HistoryServiceImpl.class);
    
    @Autowired @Qualifier(EventChannelsConfiguration.HISTORY_EVENT_CHANNEL_NAME)
    private MessageChannel historyChannel;
    
    //инстанс дефолтного конвертера полей, для ускорения обработки
    private final FieldConverter defaultConverter = new DefaultConverter();

    @Override
    public <T> void write(Principal principal, T oldObject, T newObject) throws RuntimeException {
        if (principal == null) {
            throw new IllegalArgumentException("нет данных о пользователе вызвавшем метод");
        }
        if (oldObject == null || newObject == null) {
            throw new IllegalArgumentException("журналируемый объект не может быть null");
        }
        
        //получаем аннотацию Journalable которой должна быть аннотирована журналируемая сущность
        Journalable classAnnotation = oldObject.getClass().getAnnotation(Journalable.class);        
        if (classAnnotation == null) {
            throw new IllegalArgumentException("объект не аннотирован как Journalable");
        }
        
        //TODO вытаскиваем какие то метаданные из Journalable - надо решить какие
        ServiceName serviceName = classAnnotation.value();
        if (serviceName == null) {
            throw new IllegalArgumentException("неизвестный тип сервиса");
        }
        
        HistoryEventBuilder eventBuilder = HistoryEventBuilder
                .withEntity(oldObject.getClass(), EntityConverterHelper.getEntityId(oldObject))
                .setPrincipal(principal)
                .setServiceName(serviceName);
        
        //обходим поля объекта и ищем проаннотированные
        Class objectType = oldObject.getClass();
        for (Field f : objectType.getDeclaredFields()) {
            try {
                f.setAccessible(true);
                LogIt logAnnotation = f.getAnnotation(LogIt.class);
                if (logAnnotation == null) {
                    continue;
                }
                Class<? extends FieldConverter> converterClass = logAnnotation.converter();
                FieldConverter converter;
                if (DefaultConverter.class.getName().equals(converterClass.getName())) {
                    converter = defaultConverter; //для дефолтного конвертера берем готовый инстанс
                } else {
                    converter = converterClass.newInstance(); //создаем инстанс кастомного конвертера
                }
                //вытаскиваем значения поля для старого и нового объекта
                Object oldValue = f.get(oldObject);
                Object newValue = f.get(newObject);
                //ищем измененные значения
                if (!converter.equals(oldValue, newValue)) {                
                    eventBuilder.addValue(f.getName(), converter.toString(oldValue), converter.toString(newValue));
                }
            } catch (Exception e) {
                LOG.error("ошибка логирования поля: " + f.getName() + " в классе: " + objectType.getName(), e);
                throw new RuntimeException(e);
            }
        }
        
        //отправляем событие на запись в БД
        historyChannel.send(eventBuilder.buildMessage());
    }

    @Override
    public <T> T clone(T object) throws RuntimeException {
        Class<T> objectType = (Class<T>) object.getClass();        
        try {
            T clone = objectType.newInstance();
            for (Field f : objectType.getDeclaredFields()) {
                f.setAccessible(true);
                if (f.getAnnotation(Transient.class) != null)
                    continue;
                if (Modifier.isFinal(f.getModifiers()) || Modifier.isStatic(f.getModifiers()) || Modifier.isTransient(f.getModifiers()))
                    continue; 
                Object value = f.get(object);
                Method fieldSetter = getSetterMethod(objectType, f);
                fieldSetter.invoke(clone, value);
            }
            return clone;
        } catch (Exception e) {
            LOG.error("ошибка клонирования в классе: " + objectType.getName(), e);
            throw new RuntimeException(e);
        }
    }
    
    private static Method getSetterMethod(Class objectType, Field f) {
        String methodName = null;
        try {
            methodName = "set" + f.getName().substring(0, 1).toUpperCase() + f.getName().substring(1);
            return objectType.getMethod(methodName, f.getType());
        } catch (Exception e) {
            LOG.error("отсутствует метод: " + methodName + " в классе: " + objectType.getName(), e);
            throw new RuntimeException(e);
        }
    }
    
}
