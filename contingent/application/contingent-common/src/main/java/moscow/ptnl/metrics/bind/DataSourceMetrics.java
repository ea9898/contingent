/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.metrics.bind;

import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Tag;
import io.micrometer.core.instrument.Tags;
import io.micrometer.core.instrument.binder.MeterBinder;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Objects;
import javax.sql.DataSource;

/**
 * https://www.atlantbh.com/blog/custom_metrics_micrometer_prometheus_spring_boot_actuator/
 * https://github.com/micrometer-metrics/micrometer/blob/master/micrometer-core/src/main/java/io/micrometer/core/instrument/binder/jetty/JettyStatisticsMetrics.java
 * @author m.kachalov
 */
public class DataSourceMetrics implements MeterBinder {
    
    private static final double UP = 1.0;
    private static final double DOWN = 0.0;
    private static final String SELECT_1 = "SELECT 1;";
    private static final int QUERY_TIMEOUT = 1;
    
    private final String name;
    private final String description;
    private final Iterable<Tag> tags;
    private final DataSource dataSource;
    
    public DataSourceMetrics(final DataSource dataSource) {
        Objects.requireNonNull(dataSource, "dataSource cannot be null");
        this.dataSource = dataSource;
        this.name = "data_source";
        this.description = "DataSource status";
        this.tags = tags(dataSource);
    }

    @Override
    public void bindTo(MeterRegistry meterRegistry) {
        Gauge.builder(name, this, value -> value.status() ? UP : DOWN)
                .description(description)
                .tags(tags)
                .baseUnit("status")
                .register(meterRegistry);
    }
    
    private boolean status() {
        try(Connection connection = dataSource.getConnection()) {
            PreparedStatement statement = connection.prepareStatement(SELECT_1);
            statement.setQueryTimeout(QUERY_TIMEOUT);
            statement.executeQuery();
            return true;
        } catch (SQLException ignored) {
            return false;
        }
    }
    
    protected static Iterable<Tag> tags(DataSource dataSource) {
        Objects.requireNonNull(dataSource, "dataSource cannot be null");
        try {
            return Tags.of(Tag.of("url", dataSource.getConnection().getMetaData().getURL()));
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }
    
}
