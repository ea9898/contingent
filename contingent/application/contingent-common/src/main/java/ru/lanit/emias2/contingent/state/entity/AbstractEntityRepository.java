package ru.lanit.emias2.contingent.state.entity;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import ru.lanit.emias2.contingent.state.cache.Cache;
import ru.lanit.emias2.contingent.state.cache.CacheProvider;
import ru.lanit.emias2.contingent.state.cache.CacheTransactionScope;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import moscow.ptnl.util.Strings;

/**
 * Created by mkomlev, mnasyrov on 03.05.2016.
 *
 */
public abstract class AbstractEntityRepository implements EntityRepository {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractEntityRepository.class);
    
    private final Cache cache = new CacheProvider().getCache();
    private final Map<String, EntityResolver> entityResolvers = new HashMap<>();
    private final Object sync = new Object();

    @Override
    public <T> T get(Class<T> clazz, String key) {
        if(clazz == null || key == null)
            return null;
        List<T> result = get(clazz, Arrays.asList(key));
        if(result == null || result.isEmpty())
            return null;
        else
            return result.get(0);
    }

    @Override
    public <T> List<T> get(Class<T> clazz, List<String> keys) {
        List<T> output = new ArrayList<>();
        if (clazz == null || keys == null || keys.isEmpty()) {
            return output;
        }

        keys = keys.stream().filter(c -> !Strings.isNullOrEmpty(c)).distinct().collect(Collectors.toList());
        if (keys.isEmpty()) {
            return output;
        }

        Map<String, T> results = new HashMap<>();

        for (String key : keys) {
            T content = cache.get(clazz, key);
            if(content != null) {
                results.put(key, content);
            }
        }

        List<String> nonCachedKeys = keys.stream().filter(c -> !results.containsKey(c)).collect(Collectors.toList());

        if (!nonCachedKeys.isEmpty()) {
            EntityResolver resolver = getEntityResolvers().get(clazz.getCanonicalName());

            if (resolver != null) {
                Map<String, Object> objects = resolver.getFunc().apply(nonCachedKeys);
                if (objects != null && !objects.isEmpty()) {
                    for (Map.Entry<String, Object> objectEntry : objects.entrySet()) {
                        CacheTransactionScope scope = cache.beginTransaction();
                        T cached = null;
                        try {
                            cached = cache.put(objectEntry.getKey(), (T)objectEntry.getValue(), resolver.getLifeTimeInSeconds());
                            scope.commit();
                        } catch (Exception e) {
                            scope.rollback();
                            LOG.error(String.format("Error while cache data from %s", resolver.getClass().getCanonicalName()), e);
                            throw e;
                        }
                        if(cached != null) {
                            results.put(objectEntry.getKey(), cached);
                        }
                    }
                }
            }
        }

        for (String key : keys) {
            if(results.containsKey(key)) {
                output.add(results.get(key));
            }
        }
        return output;
    }

    @Override
    public <T> T get(Class<T> clazz) {
        if(clazz == null)
            return null;
        T result = cache.get(clazz);
        if(result == null) {
            EntityResolver resolver = getEntityResolvers().get(clazz.getCanonicalName());
            if (resolver != null) {
                Map<String, Object> objects = resolver.getFunc().apply(null);
                if (objects != null && !objects.isEmpty()) {
                    CacheTransactionScope scope = cache.beginTransaction();
                    T cached = null;
                    try {
                        cached = cache.put((T)objects.values().stream().findFirst().orElse(null), resolver.getLifeTimeInSeconds());
                        scope.commit();
                    } catch (Exception e) {
                        scope.rollback();
                        LOG.error(String.format("Error while cache data from %s", resolver.getClass().getCanonicalName()), e);
                        throw e;
                    }
                    result = cached;
                }
            }
        }
        return result;
    }

    private Map<String, EntityResolver> getEntityResolvers() {
        if(entityResolvers == null) {
            synchronized (sync) {
                if(entityResolvers.isEmpty()) {                    
                    createEntityResolvers();
                }
            }
        }
        return entityResolvers;
    }
    
    /**
     * Создание резолверов источников данных.
     * Map<String, Integer> lifeTimeInSeconds = CacheLifetimeConfigProvider.getLifeTimeInSeconds(propertyService);
     * this.addEntityResolver(Employee.class.getCanonicalName(), this::getEmployee, CacheLifetimeConfigProvider.getLifeTimeInSeconds(lifeTimeInSeconds, Employee.class.getSimpleName() + "EntityResolver"));
     * this.addEntityResolver(EmployeeCacheDtoCollection.class.getCanonicalName(), this::getEmployeeCache, CacheLifetimeConfigProvider.getLifeTimeInSeconds(lifeTimeInSeconds, EmployeeCacheDtoCollection.class.getSimpleName() + "EntityResolver"));
     * this.addEntityResolver(EmployeeState.class.getCanonicalName(), this::getEmployeeState, CacheLifetimeConfigProvider.getLifeTimeInSeconds(lifeTimeInSeconds, EmployeeState.class.getSimpleName() + "EntityResolver"));
     * this.addEntityResolver(Kladr.class.getCanonicalName(), this::getKladr, CacheLifetimeConfigProvider.getLifeTimeInSeconds(lifeTimeInSeconds, Kladr.class.getSimpleName() + "EntityResolver"));
     */
    protected abstract void createEntityResolvers();


    protected void addEntityResolver(String cacheName, Function<List<String>, Map<String, Object>> resolver, int lifeTimeInSeconds) {
        entityResolvers.put(cacheName, new EntityResolver(cacheName, resolver, lifeTimeInSeconds));
    }

    static class EntityResolver {

        private final String cacheName;
        private final Function<List<String>, Map<String, Object>> func;
        private final int lifeTimeInSeconds;
        
        public EntityResolver(String cacheName, Function<List<String>, Map<String, Object>> func, int lifeTimeInSeconds) {
            this.cacheName = cacheName;
            this.func = func;
            this.lifeTimeInSeconds = lifeTimeInSeconds;
        }

        public int getLifeTimeInSeconds() {
            return lifeTimeInSeconds;
        }

        public String getCacheName() {
            return cacheName;
        }

        public Function<List<String>, Map<String, Object>> getFunc() {
            return func;
        }
    }
}