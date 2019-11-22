package moscow.ptnl.contingent.service.esu;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Service;
import ru.mos.emias.esu.consumer.EsuConsumer;
import ru.mos.emias.esu.consumer.EsuConsumerMessageProcessor;
import ru.mos.emias.esu.consumer.EsuTopicConsumer;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.management.IntrospectionException;
import javax.management.MalformedObjectNameException;
import javax.management.ReflectionException;
import java.lang.invoke.MethodHandles;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import moscow.ptnl.contingent.service.setting.SettingService;
import org.springframework.scheduling.annotation.Scheduled;

/**
 * Сервис подписки на топики и сохранения в БД сообщений от ЕСУ.
 * 
 * sorlov
 */
@Service
@PropertySource("classpath:application-esu.properties")
public class EsuConsumerService {

    private static final Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());
    
    private static final String SETTING_RUN_MODE_POSTFIX = ".run.mode";
    private static final String SETTING_RUN_BEGIN_DATE_POSTFIX = ".run.begin-date";
    private static final String SETTING_RUN_END_DATE_POSTFIX = ".run.end-date";

    private final Map<String, Consumer> consumers = new HashMap<>();
    
    @Autowired
    private EsuConsumerDatabaseProcessor esuConsumerDatabaseProcessor;

    @Value("${esu.consumer.topic.primary.area.attachment}")
    private String primaryAreaAttachmentTopicName;

    @Value("${esu.consumer.topic.job.execution.info.msg}")
    private String jobExecutionInfoMsgTopicName;

    @Value("${esu.consumer.topic.dn.event}")
    private String dnEventInformerTopicName;

    @Value("${esu.service.address}")
    private String bootstrapServers;

    @Value("${esu.consumer.group.id}")
    private String consumerGroupId;

    @Value("${esu.consumer.topic.threads.number}")
    private Integer topicThreadsNumber;

    @Value("${esu.consumer.polling.interval}")
    private Integer pollingInterval;

    @Value("${esu.consumer.polling.timeout}")
    private Integer pollingTimeout;

    @Value("${esu.consumer.error.producer.timeout}")
    private Integer producerTimeout;
    
    @Autowired
    private SettingService settingService;

    @PostConstruct
    public void subscribeTopics() {
        List<String> topics = Arrays.asList(
                jobExecutionInfoMsgTopicName, //К_УУ_ЕСУ_1
                primaryAreaAttachmentTopicName, //К_УУ_ЕСУ_3
                dnEventInformerTopicName //К_УУ_ЕСУ_5
        );
        
        topics.forEach(name -> {
            Optional<Consumer> c = createConsumer(name, esuConsumerDatabaseProcessor);
            if (c.isPresent()) {
                Consumer consumer = c.get();
                consumers.put(name, consumer);
                LOG.info("Создан подписчик топика: {}", name);
            }
        });        
    }

    @PreDestroy
    public void closeTopics() {
        consumers.entrySet().forEach(e -> {
            try {
                e.getValue().shutdown();
            } catch (Exception ex) {
                LOG.error("Не удалось остановить подписчика топика: " + e.getKey(), ex);
            }
        });
    }
    
    /**
     * Управляет включением-отключением консумеров на основе настроек из AD_CONFIG.
     * CONTINGENT2-416
     */
    @Scheduled(fixedDelay = 60000)
    private void manageTopics() {
        LOG.info("START MANAGE TOPICS");
        consumers.entrySet().forEach(e -> {
            String topic = e.getKey();
            Consumer consumer = e.getValue();
            
            if (isConsumerOn(topic)) {
                try {
                    consumer.enable();
                } catch (Exception ex) {
                    LOG.error("Ошибка запуска подписчика топика : " + topic, ex);
                }
            } else {
                try {
                    consumer.disable();
                } catch (Exception ex) {
                    LOG.error("Ошибка приостановки подписчика топика: " + topic, ex);
                }
            }
            
        });
    }
    
    /**
     * Считывает настройки из AD_CONFIG для топика и определяет нужно ли его 
     * слушать.
     * CONTINGENT2-416
     * 
     * @param topicName
     * @return 
     */
    private boolean isConsumerOn(String topicName) {
        
        //глобальная настройка прослушивателей для всех топиков
        Boolean runMode = Boolean.TRUE.equals((Boolean) settingService.getSettingProperty(SettingService.TOPICS_CONSUMERS_RUN_MODE, true));
        if (!runMode) {
            LOG.warn("Обработка топиков отключена в настройке {}", SettingService.TOPICS_CONSUMERS_RUN_MODE);
            return false; 
        }
        
        //настройка прослушивания конкретного топика
        String runModeSettingName = topicName + SETTING_RUN_MODE_POSTFIX;
        runMode = Boolean.TRUE.equals((Boolean) settingService.getSettingProperty(runModeSettingName, true));
        if (!runMode) {
            LOG.warn("Обработка топика {} отключена в настройке {}", topicName, runModeSettingName);
            return false; //если ничего не сказано, то и ничего не делаем - топик не слушаем
        }
        
        //настройки прослушивания топика в заданном временном интервале
        String beginSettingName = topicName + SETTING_RUN_BEGIN_DATE_POSTFIX;
        String endSettingName = topicName + SETTING_RUN_END_DATE_POSTFIX;
        
        LocalTime[] workInterval = null;
        try {
            LocalTime begin = settingService.getSettingProperty(beginSettingName, true);
            LocalTime end = settingService.getSettingProperty(endSettingName, true);
            // получение рабочего интервала
            workInterval = new LocalTime[]{begin, end};
            LOG.debug("Обработка топика {} возможна в интервале от {} до {}", topicName, workInterval[0], workInterval[1]);
        } catch (Exception ex) {
            LOG.error(String.format("Ожидаемый формат времени в настройке %s и %s - HH:mm:ss", beginSettingName, endSettingName), ex);
        }
    
        //определение и возврат флага возможности потребления данных топика
        return isNowInWorkInterval(workInterval);
    }
    
    /**
     * Подрезал метод из проекта ЕРП.
     * 
     * @author DGirenko
     * @param workInterval
     * @return 
     */
    private boolean isNowInWorkInterval(LocalTime[] workInterval) {
        if (workInterval == null || workInterval.length < 2 || workInterval[0] == null || workInterval[1] == null) {
            return false;
        }
        List<LocalTime[]> intervalList = new ArrayList<>();
        if (workInterval[0].isAfter(workInterval[1]) || workInterval[0].equals(workInterval[1])) {
            intervalList.add(new LocalTime[]{LocalTime.of(0, 0), workInterval[1]});
            intervalList.add(new LocalTime[]{workInterval[0], LocalTime.of(23, 59, 59, 999999999)});
        } else {
            intervalList.add(workInterval);
        }
        for (LocalTime[] timeInterval : intervalList) {
            LocalDate nowDate = LocalDate.now();
            LocalDateTime[] todayStartInterval = new LocalDateTime[]{
                LocalDateTime.of(nowDate, timeInterval[0]),
                LocalDateTime.of(nowDate, timeInterval[1])};
            LocalDateTime nowDatetime = LocalDateTime.now();
            if (nowDatetime.isAfter(todayStartInterval[0]) && nowDatetime.isBefore(todayStartInterval[1])) {
                return true;
            }
        }
        return false;
    }

    private Optional<Consumer> createConsumer(String topicName, EsuConsumerMessageProcessor processor) {
        try {
            return Optional.of(new Consumer(buildConsumer(topicName, processor), topicName));            
        } catch (Exception ex) {
            LOG.error("Не удалось создать подписчика на ЕСУ топик: " + topicName, ex);
        }
        return Optional.empty();
    }

    private EsuTopicConsumer buildConsumer(String topicName, EsuConsumerMessageProcessor processor)
            throws ReflectionException, IntrospectionException, MalformedObjectNameException {
        EsuConsumer.EsuConsumerConfig consumerConfig = EsuConsumer.config(
                // Сервера для подключения к ЕСУ Kafka
                bootstrapServers,
                // Идентификатор подписчика
                consumerGroupId,
                // Название топика
                topicName,
                // Кол-во потоков
                topicThreadsNumber,
                // Объект класса, унаследованного от EsuConsumerMessageProcessor. NOT THREAD SAFE
                processor,
                // Важность очерёдности обработки сообщений (в случае если очерёдность важна, обработка будет выполняться в один поток и при наличии ошибок обработка сообщений будет останавливаться
                false);
        consumerConfig
                // Интервал между запросами в Kafka в мс (по-умолчанию 300)
                .setPollingInterval(pollingInterval)
                // Таймаут чтения сообщений из Kafka в мс (по-умолчанию 300)
                .setPollingTimeout(pollingTimeout)
                // Таймаут продюсера, при отправке сообщений в топик ConsumerErrors
                .setProducerTimeout(producerTimeout);
        return consumerConfig.build();
    }
    
    /**
     * Состояние потребителя топика.
     */
    static enum ConsumerState {
        NOT_ACTIVE,
        ACTIVE,
        PAUSED
    }
    
    static class Consumer {
        
        private ConsumerState state;
        private final EsuTopicConsumer consumer;
        private final String name;
        
        public Consumer(EsuTopicConsumer consumer, String name) {
            this.consumer = consumer;
            this.state = ConsumerState.NOT_ACTIVE;
            this.name = name;
        }

        public ConsumerState getState() {
            return state;
        }

        public void enable() throws Exception {
            if (ConsumerState.NOT_ACTIVE.equals(state)) {
                this.consumer.consume();
                this.state = ConsumerState.ACTIVE;
                LOG.info("Запущен слушатель топика: {}", name);
            } else if (ConsumerState.PAUSED.equals(state)) {
                this.consumer.proceed();
                this.state = ConsumerState.ACTIVE;
                LOG.info("Активирован слушатель топика: {}", name);
            }
        }
        
        public void disable() throws Exception {
            if (ConsumerState.ACTIVE.equals(state)) {
                this.consumer.pause();
                this.state = ConsumerState.PAUSED;
                LOG.info("Приостановлен слушатель топика: {}", name);
            }
        }
        
        public void shutdown() throws Exception {
            this.consumer.shutdown();
            this.state = ConsumerState.NOT_ACTIVE;
            LOG.info("Завершен слушатель топика: {}", name);
        }
        
    }
}
