package moscow.ptnl.metrics.bind;

import com.sun.management.UnixOperatingSystemMXBean;
import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.binder.MeterBinder;
import java.lang.management.ManagementFactory;
import java.lang.management.OperatingSystemMXBean;
import java.lang.management.RuntimeMXBean;
import java.util.concurrent.TimeUnit;

/**
 * Необходимые разрешения для WildFly 10:
 * 
 *   &lt;module name="sun.scripting" export="true"/&gt;
 *   &lt;system export="true"&gt;
 *       &lt;paths&gt;
 *           &lt;path name="com/sun/management"/&gt;
 *       &lt;/paths&gt;
 *       &lt;exports&gt;
 *           &lt;include-set&gt;
 *               &lt;path name="META-INF/services"/&gt;
 *           &lt;/include-set&gt;
 *       &lt;/exports&gt;
 *   &lt;/system&gt;
 * 
 * Не используем FileDescriptorMetrics, так как имена параметров не соответствуют
 * постановке задачи.
 *
 * @author mkachalov
 */
public class SystemMetrics implements MeterBinder {
    
    private final UnixOperatingSystemMXBean unixOs;
    private final RuntimeMXBean runtimeMXBean;
    
    public SystemMetrics(){
        OperatingSystemMXBean osMBean = ManagementFactory.getOperatingSystemMXBean();
        if (osMBean instanceof UnixOperatingSystemMXBean) {
            this.unixOs = (UnixOperatingSystemMXBean) osMBean;
        } else {
            this.unixOs = null;
        }
        this.runtimeMXBean = ManagementFactory.getRuntimeMXBean();
    }

    @Override
    public void bindTo(MeterRegistry registry) {
        
        if (unixOs != null) {
            
            Gauge.builder("process.open.fds", unixOs, v -> v.getOpenFileDescriptorCount())            
                .description("The open file descriptor count")
                .baseUnit("files")
                .register(registry);

            Gauge.builder("process.max.fds", unixOs, v -> v.getMaxFileDescriptorCount())
                .description("The maximum file descriptor count")
                .baseUnit("files")
                .register(registry);

            Gauge.builder("process.cpu.seconds.total", unixOs, v -> {
                    double invoke = v.getProcessCpuTime();
                    return (double) TimeUnit.SECONDS.convert((long) invoke, TimeUnit.NANOSECONDS);
                })
                .description("Number of ticks executing code of that process")
                .register(registry);

            Gauge.builder("jvm_info", () -> 1.0)
                .tag("version", System.getProperty("java.runtime.version", "unknown"))
                .tag("vendor", System.getProperty("java.vm.vendor", "unknown"))
                .tag("runtime", System.getProperty("java.runtime.name", "unknown"))
                .description("JVM version info")
                .register(registry); 

            Gauge.builder("system_cpu_count", unixOs, v -> v.getAvailableProcessors())
                .description("Number of available processors")
                .register(registry);

            Gauge.builder("system_memory_physical_free", unixOs, v -> v.getFreePhysicalMemorySize())
                .description("Free physical memory")
                .baseUnit("bytes")
                .register(registry);

            Gauge.builder("system_memory_physical_total", unixOs, v -> v.getTotalPhysicalMemorySize())
                .description("Total physical memory")
                .baseUnit("bytes")
                .register(registry);

            Gauge.builder("system_swap_free", unixOs, v -> v.getFreeSwapSpaceSize())
                .description("Free swap space size")
                .baseUnit("bytes")
                .register(registry);

            Gauge.builder("system_swap_total", unixOs, v -> v.getTotalSwapSpaceSize())
                .description("Total swap space size")
                .baseUnit("bytes")
                .register(registry);

            Gauge.builder("system_loadaverage", unixOs, v -> v.getSystemLoadAverage())
                .description("System load average")
                .register(registry);
            
        }
        
        Gauge.builder("process.uptime.seconds", runtimeMXBean, v -> {
                return (double) TimeUnit.SECONDS.convert(v.getUptime(), TimeUnit.MILLISECONDS);
            })
            .description("The uptime of the Java virtual machine")
            .register(registry);

        Gauge.builder("process.start.time.seconds", runtimeMXBean, v -> {
                return (double) TimeUnit.SECONDS.convert(v.getStartTime(), TimeUnit.MILLISECONDS);
            })                
            .description("Start time of the process since unix epoch.")
            .register(registry);       
        
    }
    
}
