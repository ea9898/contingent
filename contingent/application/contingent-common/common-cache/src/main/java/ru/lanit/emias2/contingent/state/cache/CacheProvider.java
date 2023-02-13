package ru.lanit.emias2.contingent.state.cache;

import java.io.InputStream;
import java.util.Properties;

/**
 * Created by mkomlev on 04.05.2016.
 *
 */
public final class CacheProvider {

    private static Cache cache;
    private static final Object sync = new Object();
    private static final String CACHE_NAME = "provider";
    private static final String CACHE_SETTINGS = "settings";
    private static final String CACHE_DEFAULT = "memory";

    public Cache getCache() {
        if (cache == null) {
            synchronized (sync) {
                if (cache == null) {
                    Properties properties = getProperties();
                    cache = resolveCache(properties);
                }
            }
        }

        return cache;
    }

    private Cache resolveCache(Properties properties) {
        String cacheName = (String) properties.getOrDefault(CACHE_NAME, CACHE_DEFAULT);
        String settings = (String) properties.getOrDefault(CACHE_SETTINGS, null);
        switch (cacheName) {
            case "redis":
                throw new IllegalArgumentException("unsupported cache provider type: \"redis\"");
                //return new RedisCache(settings);
            case "redis-cluster":
                throw new IllegalArgumentException("unsupported cache provider type: \"redis-cluster\"");
                //return new RedisClusterCache(settings);
            case "infinispan":
                return new InfinispanCache(settings);
            default:
                return new InMemoryCache();
        }
    }

    private Properties getProperties() {
        Properties properties = new Properties();
        try {
            InputStream stream = getClass().getClassLoader().getResourceAsStream("cache.properties");
            if (stream != null) {
                properties.load(stream);
                stream.close();
            }
        } catch (Exception e) {
            properties.clear();
            properties.put(CACHE_NAME, CACHE_DEFAULT);
        }
        return properties;
    }
}
