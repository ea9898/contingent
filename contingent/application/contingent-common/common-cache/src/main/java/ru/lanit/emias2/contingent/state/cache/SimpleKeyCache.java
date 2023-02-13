package ru.lanit.emias2.contingent.state.cache;

/**
 * Created by mkomlev on 05.05.2016.
 */
public abstract class SimpleKeyCache extends BaseCache {
    protected final static String SEPARATOR = "::";
    protected abstract String getInternal(String key);
    protected abstract void putInternal(String key, String value, int lifeTimeInSeconds);

    @Override
    protected String getInternal(String cacheName, String key) {
        return getInternal(cacheName + SEPARATOR + key);
    }

    @Override
    protected void putInternal(String cacheName, String key, String value, int lifeTimeInSeconds) {
        putInternal(cacheName + SEPARATOR + key, value, lifeTimeInSeconds);
    }
}
