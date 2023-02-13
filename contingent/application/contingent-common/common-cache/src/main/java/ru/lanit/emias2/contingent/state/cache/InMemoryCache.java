package ru.lanit.emias2.contingent.state.cache;

import jakarta.transaction.TransactionManager;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * @author mkomlev
 */
public class InMemoryCache extends SimpleKeyCache implements Cache {

    private final ConcurrentMap<String, String> cache = new ConcurrentHashMap<>();

    @Override
    protected String getInternal(String key) {
        return cache.get(key);
    }

    @Override
    protected void putInternal(String key, String value, int lifeTimeInSeconds) {
        cache.put(key, value);
    }

    @Override
    protected void clearInternal(String cacheName) {
        //cache.keySet().stream().filter(c -> cacheName.equals(c) || c.startsWith(cacheName + SEPARATOR)).forEach(cache::remove);
    }

    @Override
    public TransactionManager getTransactionManager() {
        return null;
    }
}
