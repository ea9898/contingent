package moscow.ptnl.contingent.nsi.service;

import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.nsi.domain.NsiFormConstraint;
import moscow.ptnl.contingent.nsi.domain.NsiFormTablesEnum;
import moscow.ptnl.contingent.nsi.ws.security.UserContextHolder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.integration.support.MessageBuilder;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Service;
import ru.mos.emias.system.v1.usercontext.UserContext;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Queue;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import static moscow.ptnl.contingent.nsi.configuration.Constraint.NSI_FORM_REQUEST_CHANNEL_NAME;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Service
public class NsiAdminService {
    
    private static final Logger LOG = LoggerFactory.getLogger(NsiAdminService.class);

    @Autowired
    private SettingService settingService;

    @Autowired
    @Qualifier(NSI_FORM_REQUEST_CHANNEL_NAME)
    private MessageChannel nsiRequestChannel;
    
    
    public List<Long> updateAddressesByGlobalId(Long formId, List<Long> globalIds, NsiFormTablesEnum entityType) {
        Queue<Long> unrecognizedAddresses = new ConcurrentLinkedQueue<>();
        long threadsNumber = settingService.getSettingProperty(SettingService.UPDATE_ADDRESS_BY_GLOBAL_ID_THREADS);
        UserContext context = UserContextHolder.getUserContext();
        ExecutorService executor = Executors.newFixedThreadPool((int) threadsNumber);

        try {
            CompletableFuture<?>[] futures = globalIds.stream()
                    .filter(Objects::nonNull)
                    .distinct()
                    .map(id -> CompletableFuture.runAsync(() -> nsiRequestChannel.send(MessageBuilder
                            .withPayload(id)
                            .setHeader(NsiFormConstraint.FORM_ID_HEADER, formId)
                            .setHeader(NsiFormConstraint.ENTITY_TYPE_HEADER, entityType)
                            .setHeader(NsiFormConstraint.USER_CONTEXT, context)
                            .setHeader(NsiFormConstraint.UNRECOGNIZED_ADDRESSES, unrecognizedAddresses)
                            .build()), executor)
                    ).toArray(CompletableFuture<?>[]::new);
            CompletableFuture<?> future = CompletableFuture.allOf(futures);
            Stream.of(futures).forEach(f ->
                    f.exceptionally(e -> {
                        //Прекращаем выполнение, если ошибка хотя бы в одном потоке
                        future.completeExceptionally(e);
                        return null;
                    }));
            future.join();
        } finally {
            shutdownExecutor(executor);
        }
        return new ArrayList<>(unrecognizedAddresses);
    }
    
    private static void shutdownExecutor(ExecutorService executor) {
        executor.shutdown();
        try {
            executor.awaitTermination(100, TimeUnit.MILLISECONDS);
        } catch (InterruptedException e) {
            LOG.error("Ошибка закрытия пула потоков", e);
        } finally {
            executor.shutdownNow();
        }
    }
    
}
