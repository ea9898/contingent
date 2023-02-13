package area.service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import jakarta.annotation.PostConstruct;
import moscow.ptnl.contingent.domain.esu.ESUEventHelper;
import moscow.ptnl.contingent.esu.service.EsuService;
import org.springframework.stereotype.Service;

/**
 * Заглушка сервиса получающего сообщение для отправки в ЕСУ и сохранения в БД.
 * Полученное сообщение 
 * 
 * @author m.kachalov
 */
@Service
public class MockEsuService implements EsuService {
    
    private final List<MockMessage> messages = new ArrayList<>();    

    @Override
    public boolean saveAndPublishToESU(String topicName, Object event) {        
        this.messages.add(new MockMessage(topicName, ESUEventHelper.toESUMessage(event, 1L)));
        return true;
    }

    @Override
    public void periodicalPublishUnsuccessMessagesToESU(LocalDateTime olderThan) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void publishToESU(Long recordId, String publishTopic, String message) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    public boolean hasMessage() {
        return !this.messages.isEmpty();
    } 
    
    /**
     * Первое сообщение из полученных, если их более одного.
     * 
     * @return 
     */
    public MockMessage getMessage() {
        return (hasMessage()) ? this.messages.get(0) : null;
    }
    
    public List<MockMessage> getMessages() {
        return this.messages;
    }
    
    @PostConstruct
    public void init() {
        this.messages.clear();
    }
    
    public static class MockMessage {
        
        private final String topicName;
        private final String message;
        
        public MockMessage(String topicName, String message) {
            this.topicName = topicName;
            this.message = message;
        }
        
        public String getTopicName() {
            return topicName;
        }

        public String getMessage() {
            return message;
        }
    }
    
}
