package moscow.ptnl.contingent.esu.producer;

/**
 * Набор свойств необходимых продюсеру сообщений в Kafka.
 * 
 * @author m.kachalov
 */
public interface ProducerProperties {
    
    String getBootstrapServers();
    
    String getProducerId();
    
    /**
     * Задает request.timeout.ms.
     * Рекомендуемое значение 60000 мсек, минимальное значение > 20000 мсек.
     * 
     * @return мсек
     */
    Integer getDeliveryTimeout();
    
    /**
     * Разрешить логирование метрик.
     * 
     * @return 
     */
    default boolean isLogEnabled() {
        return false;
    }
    
    /**
     * Сервера куда ведется логирование метрик.
     * 
     * @return 
     */
    default String getLogServers() {
        return null;
    }
    
    /**
     * Идентификатор продукта на сервере логирования метрик.
     * 
     * @return 
     */
    default String getLogProducerId() {
        return null;
    }
    
}
