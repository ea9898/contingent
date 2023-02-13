package ru.lanit.emias2.contingent.state.cache;

import jakarta.transaction.TransactionManager;

/**
 * Интерфейс кеша.
 * 
 * @author mkomlev
 */
public interface Cache {
    
    TransactionManager getTransactionManager();
    
    default CacheTransactionScope beginTransaction() {
        return new CacheTransactionScope(getTransactionManager());
    }
    
    String get(CacheName cacheName);
    
    <T> T get(Class<T> valueClass);
    
    <T> T get(CacheName cacheName, Class<T> valueClass);
    
    <T> T get(Class<T> valueClass, String key);
    
    <T> T get(CacheName cacheName, Class<T> valueClass, String key);
    
    default <T> T get(Class<T> valueClass, long key) {
        return get(valueClass, String.valueOf(key));
    }
    
    default <T> T get(CacheName cacheName, Class<T> valueClass, long key) {
        return get(cacheName, valueClass, String.valueOf(key));
    }
    
    default <T> T get(Class<T> valueClass, int key) {
        return get(valueClass, String.valueOf(key));
    }
    
    default <T> T get(CacheName cacheName, Class<T> valueClass, int key) {
        return get(cacheName, valueClass, String.valueOf(key));
    }
    
    default <T> T get(Class<T> valueClass, Long key) {
        return get(valueClass, String.valueOf(key));
    }
    
    default <T> T get(CacheName cacheName, Class<T> valueClass, Long key) {
        return get(cacheName, valueClass, String.valueOf(key));
    }
    
    default <T> T get(Class<T> valueClass, Integer key) {
        return get(valueClass, String.valueOf(key));
    }
    
    default <T> T get(CacheName cacheName, Class<T> valueClass, Integer key) {
        return get(cacheName, valueClass, String.valueOf(key));
    }


    String put(CacheName cacheName, String value);
    
    <T> T put(T value);
    
    <T> T put(CacheName cacheName, T value);
    
    <T> T put(String key, T value);
    
    <T> T put(CacheName cacheName, String key, T value);
    
    String put(CacheName cacheName, String value, int lifeTimeInSeconds);
    
    <T> T put(T value, int lifeTimeInSeconds);
    
    <T> T put(CacheName cacheName, T value, int lifeTimeInSeconds);
    
    <T> T put(String key, T value, int lifeTimeInSeconds);
    
    <T> T put(CacheName cacheName, String key, T value, int lifeTimeInSeconds);

    default <T> T put(long key, T value) {
        return put(String.valueOf(key), value);
    }
    
    default <T> T put(CacheName cacheName, long key, T value) {
        return put(cacheName, String.valueOf(key), value);
    }
    
    default <T> T put(int key, T value) {
        return put(String.valueOf(key), value);
    }
    
    default <T> T put(CacheName cacheName, int key, T value) {
        return put(cacheName, String.valueOf(key), value);
    }
    
    default <T> T put(long key, T value, int lifeTimeInSeconds) {
        return put(String.valueOf(key), value, lifeTimeInSeconds);
    }
    
    default <T> T put(CacheName cacheName, long key, T value, int lifeTimeInSeconds) {
        return put(cacheName, String.valueOf(key), value, lifeTimeInSeconds);
    }
    
    default <T> T put(int key, T value, int lifeTimeInSeconds) {
        return put(String.valueOf(key), value, lifeTimeInSeconds);
    }
    
    default <T> T put(CacheName cacheName, int key, T value, int lifeTimeInSeconds) {
        return put(cacheName, String.valueOf(key), value, lifeTimeInSeconds);
    }
    
    default <T> T put(Long key, T value) {
        return put(String.valueOf(key), value);
    }
    
    default <T> T put(CacheName cacheName, Long key, T value) {
        return put(cacheName, String.valueOf(key), value);
    }
    
    default <T> T put(Integer key, T value) {
        return put(String.valueOf(key), value);
    }
    
    default <T> T put(CacheName cacheName, Integer key, T value) {
        return put(cacheName, String.valueOf(key), value);
    }
    
    default <T> T put(Long key, T value, int lifeTimeInSeconds) {
        return put(String.valueOf(key), value, lifeTimeInSeconds);
    }
    
    default <T> T put(CacheName cacheName, Long key, T value, int lifeTimeInSeconds) {
        return put(cacheName, String.valueOf(key), value, lifeTimeInSeconds);
    }
    
    default <T> T put(Integer key, T value, int lifeTimeInSeconds) {
        return put(String.valueOf(key), value, lifeTimeInSeconds);
    }
    
    default <T> T put(CacheName cacheName, Integer key, T value, int lifeTimeInSeconds) {
        return put(cacheName, String.valueOf(key), value, lifeTimeInSeconds);
    }

    void clear(Class<?> valueClass);
    
    void clear(CacheName cacheName);
}