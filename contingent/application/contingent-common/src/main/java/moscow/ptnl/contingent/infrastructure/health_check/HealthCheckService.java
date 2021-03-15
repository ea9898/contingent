package moscow.ptnl.contingent.infrastructure.health_check;

import moscow.ptnl.contingent.dao.DatabaseCheckDao;

import org.apache.kafka.clients.admin.AdminClient;
import org.apache.kafka.clients.admin.ListTopicsOptions;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.serialization.StringSerializer;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.AsyncResult;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

/**
 * @author sorlov
 */
@Service
@PropertySource("classpath:application.properties")
public class HealthCheckService {

    @Value("${esu.producer.produce.timeout}")
    private Integer producerRequestTimeout;

    @Value("${esu.service.address}")
    private String esuServers;

    @Value("${health.check.tables}")
    private String tables;

    @Autowired
    private DatabaseCheckDao databaseCheckDao;

    @Async
    @Transactional(propagation = Propagation.REQUIRED)
    public Future<String> databaseHealthCheck() {
        List<String> tablesList = Arrays.asList(tables.split(","));
        return new AsyncResult<>(tablesList.stream()
                .filter(t -> !t.startsWith("temp_") && !t.contains("_temp_"))
                .map(t -> {
                    long startTime = System.currentTimeMillis();
                    return "Fetch number of records " + databaseCheckDao.selectCountFromTable(t) +
                            " from " + t + " in " + (System.currentTimeMillis() - startTime) + "ms";
                })
                .collect(Collectors.joining("\n"))
        );
    }

    public String esuHealthCheck() {
        Map<String, Object> props = new HashMap<>();
        // Bootstrap server list, separated by commas [,]
        props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, esuServers);
        // Key serializer class. Default - StringSerializer.class
        props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class);
        // Message body serializer class. Default - StringSerializer.class
        props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, StringSerializer.class);
        // Producer timeout
        props.put(ProducerConfig.REQUEST_TIMEOUT_MS_CONFIG, producerRequestTimeout);

        long startTime = System.currentTimeMillis();

        try (AdminClient client = AdminClient.create(props)) {
            client.listTopics(new ListTopicsOptions().timeoutMs(producerRequestTimeout)).listings().get();
        } catch (InterruptedException | ExecutionException e) {
            throw new RuntimeException("Kafka servers are not accessible", e);
        }
        return "Connected to Kafka in " + (System.currentTimeMillis() - startTime) + "ms";
    }
}
