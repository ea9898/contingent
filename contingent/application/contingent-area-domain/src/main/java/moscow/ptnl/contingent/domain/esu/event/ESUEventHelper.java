package moscow.ptnl.contingent.domain.esu.event;

import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import moscow.ptnl.contingent2.attachment.changearea.event.AttachOnAreaChange;
import moscow.ptnl.contingent2.attachment.deparea.event.AttachToDependentAreaEvent;
import moscow.ptnl.util.XMLUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author mkachalov
 */
public class ESUEventHelper {
    
    private final static Logger LOG = LoggerFactory.getLogger(ESUEventHelper.class);
    
    private ESUEventHelper(){}
    
    /**
     * Конвертируем объект в строковое представление для записи в ЕСУ.
     * 
     * @param publishObject
     * @param eventId
     * @return
     * @throws IllegalArgumentException
     */
    public static String toESUMessage(Object publishObject, Long eventId) throws IllegalArgumentException {
        if (publishObject == null) {
            throw new IllegalArgumentException("объект не может быть null");
        }
        if (publishObject instanceof AreaInfoEvent) {
            AreaInfoEvent event = (AreaInfoEvent) publishObject;
            event.setId(eventId);
            if (event.getOperationDate() == null) {
                event.setOperationDate(XMLUtil.getCurrentDate());
            }
            return XMLUtil.convertEventObjectToMessage(event, event.getClass());
        } else if (publishObject instanceof AttachOnAreaChange) {
            AttachOnAreaChange event = (AttachOnAreaChange) publishObject;
            event.setId(eventId);
            if (event.getOperationDate() == null) {
                event.setOperationDate(XMLUtil.getCurrentDate());
            }
            return XMLUtil.convertEventObjectToMessage(event, event.getClass());
        } else if (publishObject instanceof AttachToDependentAreaEvent) {
            AttachToDependentAreaEvent event = (AttachToDependentAreaEvent) publishObject;
            event.setId(eventId);
            if (event.getOperationDate() == null) {
                event.setOperationDate(XMLUtil.getCurrentDate());
            }
            return XMLUtil.convertEventObjectToMessage(event, event.getClass());
        }
        throw new IllegalArgumentException("неподдерживаемый тип события: " + publishObject.getClass().getName());
    }
    
    /**
     * Определяем подходящий топик в зависимости от типа события.
     * 
     * @param publishObject
     * @return 
     * @throws IllegalArgumentException
     */
    public static String resolveTopicName(Object publishObject) throws RuntimeException { 
        if (publishObject == null) {
            throw new IllegalArgumentException("событие не может быть null");
        }
        String topicName = null;
        LOG.debug("\n================\nEVENT TYPE: \n==================", publishObject.getClass().getName()); 
        
        //TODO определить топик по типу события        
        if (publishObject instanceof moscow.ptnl.contingent2.area.info.AreaInfoEvent) {
            topicName = "areaInfoEvent";
        } else if (publishObject instanceof AttachToDependentAreaEvent) {
            topicName = "attachToDependentAreaEvent";
        }
        
        if (topicName == null) {
            throw new IllegalArgumentException("не удалось определить имя топика для события: " + publishObject.getClass().getName());
        }
        return topicName;
    }
    
}
