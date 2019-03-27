package ru.lanit.emias2.contingent.state.cache;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import com.fasterxml.jackson.datatype.jsr310.JSR310Module;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import moscow.ptnl.util.Strings;

/**
 * Created by mkomlev on 04.05.2016.
 *
 */
abstract class BaseCache implements Cache {
    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper().registerModule(new JSR310Module());
    private final static String DEFAULT_KEY = "default-value";
    private final Logger log;

    BaseCache() {
        log = LoggerFactory.getLogger(getClass());
    }

    protected abstract String getInternal(String cacheName, String key);
    protected abstract void putInternal(String cacheName, String key, String value, int lifeTimeInSeconds);
    protected abstract void clearInternal(String cacheName);

    @Override
    public String get(CacheName cacheName) {
        return getInternal(cacheName.getName(), DEFAULT_KEY);
    }

    @Override
    public <T> T get(CacheName cacheName, Class<T> valueClass, String key) {
        if (Strings.isNullOrEmpty(key)) {
            return null;
        }

        if (cacheName == null) {
            cacheName = CacheName.get(valueClass.getCanonicalName());
        }

        String content = getInternal(cacheName.getName(), key);

        if (Strings.isNullOrEmpty(content)) {
            return null;
        }

        return deserialize(content, valueClass);
    }

    @Override
    public <T> T get(Class<T> valueClass) {
        return get((CacheName) null, valueClass);
    }

    @Override
    public <T> T get(CacheName cacheName, Class<T> valueClass) {
        return get(cacheName, valueClass, DEFAULT_KEY);
    }

    @Override
    public <T> T get(Class<T> valueClass, String key) {
        return get(null, valueClass, key);
    }

    @Override
    public String put(CacheName cacheName, String value) {
        return put(cacheName, value, -1);
    }

    @Override
    public <T> T put(T value) {
        return put(value, -1);
    }

    @Override
    public <T> T put(CacheName cacheName, T value) {
        return put(cacheName, value, -1);
    }

    @Override
    public <T> T put(String key, T value) {
        return put(key, value, -1);
    }

    @Override
    public <T> T put(CacheName cacheName, String key, T value) {
        return put(cacheName, key, value, -1);
    }

    @Override
    public String put(CacheName cacheName, String value, int lifeTimeInSeconds) {
        putInternal(cacheName.getName(), DEFAULT_KEY, value, lifeTimeInSeconds);
        return value;
    }

    @Override
    public <T> T put(T value, int lifeTimeInSeconds) {
        return put((CacheName)null, value, lifeTimeInSeconds);
    }

    @Override
    public <T> T put(CacheName cacheName, T value, int lifeTimeInSeconds) {
        return put(cacheName, DEFAULT_KEY, value, lifeTimeInSeconds);
    }

    @Override
    public <T> T put(String key, T value, int lifeTimeInSeconds) {
        return put(null, key, value, lifeTimeInSeconds);
    }

    @Override
    public <T> T put(CacheName cacheName, String key, T value, int lifeTimeInSeconds) {
        if (value == null || Strings.isNullOrEmpty(key)) {
            return null;
        }

        if (cacheName == null) {
            cacheName = CacheName.get(value.getClass().getCanonicalName());
        }

        String content = serialize(value);
        putInternal(cacheName.getName(), key, content, lifeTimeInSeconds);

        return value;
    }

    @Override
    public void clear(Class<?> valueClass) {
        clear(CacheName.get(valueClass.getCanonicalName()));
    }

    @Override
    public void clear(CacheName cacheName) {
        clearInternal(cacheName.getName());
    }

    private <T> T deserialize(String source, Class<T> clazz) {
        if (Strings.isNullOrEmpty(source)) {
            return null;
        }
        try {
            return OBJECT_MAPPER.readValue(source, clazz);
        } catch (IOException e) {
            log.error("Cache deserialize error", e);
            return null;
        }
    }

    private <T> String serialize(T source) {
        if (source == null) {
            return null;
        }
        try {
            return OBJECT_MAPPER.writeValueAsString(source);
        } catch (JsonProcessingException e) {
            log.error("Cache serialize error", e);
            return null;
        }
    }
}
