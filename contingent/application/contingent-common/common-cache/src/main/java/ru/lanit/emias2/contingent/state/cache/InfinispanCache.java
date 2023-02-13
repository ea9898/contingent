package ru.lanit.emias2.contingent.state.cache;

import org.infinispan.manager.CacheContainer;

import javax.naming.InitialContext;
import jakarta.transaction.TransactionManager;
import java.util.concurrent.TimeUnit;

/**
 * Created by mkomlev on 04.05.2016.
 *
 */
class InfinispanCache extends BaseCache implements Cache {
    
    private final org.infinispan.Cache<String, String> cache;

    InfinispanCache(String settings) {
        CacheContainer cacheContainer = null;
        try {
            cacheContainer = InitialContext.doLookup("java:jboss/infinispan/container/" + settings);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        this.cache = cacheContainer.getCache(); // работаем с дефолтовым кешем из контейнера
    }

    @Override
    protected String getInternal(String cacheName, String key) {
        return this.cache.get(buildEntryKey(cacheName, key));
    }

    @Override
    protected void putInternal(String cacheName, String key, String value, int lifeTimeInSeconds) {
        if (lifeTimeInSeconds > 0) {
            this.cache.put(buildEntryKey(cacheName, key), value, lifeTimeInSeconds, TimeUnit.SECONDS);
        } else {
            this.cache.put(buildEntryKey(cacheName, key), value);
        }
    }

    private String buildEntryKey(String cacheName, String key) {
        return String.format("%s#%s", cacheName, key);
    }

    @Override
    public void clearInternal(String cacheName) {
//        final String cacheNameFilter = String.format("%s#", cacheName);
//        List<String> keysToClear = this.cache.getAdvancedCache().getDataContainer().keySet().stream().filter(c -> c.startsWith(cacheNameFilter)).collect(Collectors.toList());
//        for (String key : keysToClear) {
//            this.cache.remove(key);
//        }
    }

    @Override
    public TransactionManager getTransactionManager() {
        return this.cache.getAdvancedCache().getTransactionManager();
    }
}