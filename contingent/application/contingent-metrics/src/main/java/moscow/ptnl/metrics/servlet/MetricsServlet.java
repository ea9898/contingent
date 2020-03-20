/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.metrics.servlet;

import io.micrometer.prometheus.PrometheusMeterRegistry;
import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.exporter.common.TextFormat;
import java.io.IOException;
import java.io.Writer;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Модифицированная версия сервлета.
 * Модификация (добавлены геттеры и сеттеры) позволяет инициализировать метрики 
 * после инициализации сервлета.
 * 
 * https://github.com/prometheus/client_java/blob/master/simpleclient_servlet/src/main/java/io/prometheus/client/exporter/MetricsServlet.java
 * @author m.kachalov
 */
public class MetricsServlet extends HttpServlet {
        
    private PrometheusMeterRegistry meterRegistry;
    
    /**
     * @return the registry
     */
    public CollectorRegistry getRegistry() {
        return meterRegistry.getPrometheusRegistry();
    }

    /**
     * @param meterRegistry the registry to set
     */
    public void setMeterRegistry(PrometheusMeterRegistry meterRegistry) {
        this.meterRegistry = meterRegistry;
    }

    @Override
    protected void doGet(final HttpServletRequest req, final HttpServletResponse resp)
            throws ServletException, IOException {
        resp.setStatus(HttpServletResponse.SC_OK);
        resp.setHeader("Cache-Control", "private, no-store, no-cache, must-revalidate");
        resp.setHeader("Pragma", "no-cache");
        resp.addDateHeader("Last-Modified", System.currentTimeMillis());
        resp.setContentType(TextFormat.CONTENT_TYPE_004);

        try (Writer writer = resp.getWriter()) {
            if (getRegistry() != null) {
                TextFormat.write004(writer, getRegistry().filteredMetricFamilySamples(parse(req)));
            }
            writer.flush();
        }
    }

    private Set<String> parse(HttpServletRequest req) {
        String[] includedParam = req.getParameterValues("name[]");
        if (includedParam == null) {
            return Collections.emptySet();
        } else {
            return new HashSet<>(Arrays.asList(includedParam));
        }
    }

    @Override
    protected void doPost(final HttpServletRequest req, final HttpServletResponse resp)
            throws ServletException, IOException {
        doGet(req, resp);
    }
    
}
