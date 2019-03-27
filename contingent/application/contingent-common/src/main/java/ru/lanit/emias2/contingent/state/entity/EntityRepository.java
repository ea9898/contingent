package ru.lanit.emias2.contingent.state.entity;

import java.util.List;

/**
 * Репозиторий для получения сущностей по коду.
 * 
 * Created by mkomlev on 03.05.2016.
 */
public interface EntityRepository {
    /**
     * Получить сущность по ключу.
     * 
     * @param <T>
     * @param clazz
     * @param key ключ
     * @return значение или null
     */
    <T> T get(Class<T> clazz, String key);

    /**
     * Получить список сущностей по списку ключей.
     * 
     * @param <T>
     * @param clazz
     * @param keys ключи
     * @return список сущностей, найденных по ключам
     */
    <T> List<T> get(Class<T> clazz, List<String> keys);

    /**
     * Получить сущность.
     * 
     * @param <T>
     * @param clazz
     * @return значение или null
     */
    <T> T get(Class<T> clazz);
}
