package ru.lanit.emias2.contingent.state.cache;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * Created by mkomlev on 04.05.2016.
 *
 */
public class CacheName {
    
    private final String name;

    private final static ConcurrentMap<String, CacheName> map = new ConcurrentHashMap<>();

    private CacheName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static CacheName get(String name) {
        CacheName v = map.get(name);
        if (v == null) {
            map.put(name, v = new CacheName(name));
        }
        return v;
    }

    @Override
    public String toString() {
        return getName();
    }
}
