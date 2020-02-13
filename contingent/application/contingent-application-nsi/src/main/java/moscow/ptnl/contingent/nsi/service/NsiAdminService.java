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

import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Stream;

import static moscow.ptnl.contingent.nsi.configuration.Constraint.NSI_FORM_REQUEST_CHANNEL_NAME;

@Service
public class NsiAdminService {

    @Autowired
    private SettingService settingService;

    @Autowired
    @Qualifier(NSI_FORM_REQUEST_CHANNEL_NAME)
    private MessageChannel nsiRequestChannel;

    public void updateAddressesByGlobalId(Long formId, List<Long> globalIds, NsiFormTablesEnum entityType) {
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
        }
        finally {
            executor.shutdownNow();
        }
    }
}
